{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}

module Synthesis where

import Import
import Language
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import Control.Monad.Heap
import Control.Monad.State
import Control.Monad.Reader
import Data.Monus.Dist
import qualified Prettyprinter as Pretty

-- TODO: maybe we also want to keep track of a list of possible refinements
-- e.g. [Term Unit]
data SynState = SynState
  { _contexts    :: Map Hole HoleCtx
  , _constraints :: Logic Constraints
  , _fillings    :: Map Hole (Term Hole)
  , _freshSt     :: FreshState
  , _mainScope   :: Scope
  , _examples    :: [Assert]
  , _included    :: [(Var, Poly)]
  , _forbidden   :: [Map Hole (Term Unit)]

  , _varOnly     :: Set Hole
  , _scrutinized :: Set Var
  , _multiplier  :: Map Hole Dist
  }

makeLenses ''SynState

emptySynState :: SynState
emptySynState = SynState mempty (Disjunction []) mempty mkFreshState
  mempty [] mempty [] mempty mempty mempty

weigh :: Hole -> Dist -> Synth ()
weigh h d = do
  tell d
  m <- Map.lookup h <$> use multiplier
  tell $ d * fromMaybe 1 m

-- TODO: maybe Prob is better than Dist, since we might want to use fractions
-- for multipliers.
newtype Nondet a = Nondet { runNondet :: Heap Dist a }
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (Alternative, MonadPlus, MonadWriter Dist)

instance MonadFail Nondet where
  fail _ = mzero

runSynth :: Env -> Synth a -> Nondet a
runSynth m x = fst <$> runReaderT (runStateT x emptySynState) m

instance HasFreshState SynState where
  freshState = freshSt

type Synth = StateT SynState (ReaderT Env Nondet)

instance LiftEval Synth where
  liftEval x = runIdentity . runReaderT x <$> view (env . scope)

-- | Retrieve the context of a hole.
getCtx :: Hole -> Synth HoleCtx
getCtx h = use contexts >>=
  maybe (fail $ "Missing holeCtx for hole " <> show h) return . Map.lookup h

instance ExpandHole Hole Synth where
  expandHole h = do
    (xs, ctx') <- getCtx h >>= expandHole
    modifying contexts $ Map.insert h ctx'
    return (xs, h)

liftUneval :: Int -> Uneval a -> Synth (Maybe a)
liftUneval fuel x = ask <&> \e -> view _1 <$> runRWST x e fuel

-- TODO: figure out how to deal with diverging unevaluation, such as that of
-- 'foldList {} (\x r -> r) {}' onto some examples, like '\[] -> 0', or for
-- example 'mult 0 {0} <= 0'.
-- NOTE: it seems to keep trying to merge more and more elaborate examples
-- This seems to be a very specific example where unevaluation diverges, which
-- is usually not encountered during synthesis, but only in these specific
-- circumstances?

-- TODO: make sure that local type unifications don't leak to other places. For
-- example, when synthesizing 'map', we might define 'map_succ' (a helper
-- function for assertions) as 'map Succ', but this instantiates the type
-- variables of 'map' to 'Nat', which is incorrect. Not only does this lead to
-- the type incorrect synthesis of 'map' specialized to 'Succ', but if we had
-- another helper function such as 'map_unit = map (const Unit)', the resulting
-- unification error would stop synthesis from occuring altogether.

-- TODO: implement a function that reads and type checks the program correctly,
-- without leaking type information between functions. To what extend do we
-- consider the toplevel functions part of the global/local environment? It is
-- not great to mix the current local scope (based on unevaluation constraints
-- from the assertions) with the semi-local scope of toplevel functions (which
-- might still contain holes and are not necessarily globally available in
-- every hole context). The resulting function should be somewhat similar to
-- the fromDefs function in Language.Live.hs, in that it folds over all the
-- toplevel definitions.
-- ALTERNATIVELY: implement full let polymorphism.

init :: Defs Unit -> Synth (Term Hole)
init defs = do
  let ws = [x | Include xs <- pragmas defs, x <- toList xs]

  gs <- asks $ view functions
  ws' <- forOf (each . _2 . each) ws refresh

  assign included $ ws' <&> \(v, a) ->
    ( v
    , case Map.lookup v gs of
        Nothing -> error $ "Unknown function " <> show v
        Just b -> case a of
          Nothing -> b
          Just (Poly _ t) | Poly _ u <- b -> case unify t u of
            Nothing ->
              error $ "Included function " <> show v <> " has wrong type"
            Just th -> poly (subst th t)
    )

  let addBinding (MkBinding a x) = Let a x
  (x, _, ctx) <-
    infer' mempty . foldr addBinding (Hole (Unit ())) . bindings $ defs
  let ss = Map.fromList $ signatures defs <&> \(MkSignature a t) -> (a, t)
  fs <- failMaybe $ view localEnv . fst <$> Map.maxView ctx
  th <- failMaybe . unifies $
    Map.intersectionWith (,) (fs <&> \(Poly _ t) -> t) (ss <&> \(Poly _ u) -> u)
  -- TODO: make sure the correct variables are frozen!
  let ctx' = ctx <&>
        over goalType freezeAll
        . over localEnv (freezeUnbound . instantiate th <$>)
        . subst th
  assign contexts ctx'
  y <- etaExpand id (strip x)
  liftEval (eval mempty y) >>= \case
    Scoped m (Hole h) -> do
      assign mainScope m
      modifying contexts $ Map.delete h
      assign examples $ asserts defs
      -- TODO: find reasonable fuel
      cs <- liftUneval 1000 (for (asserts defs) (unevalAssert m)) >>= \case
        Nothing -> fail "Out of fuel"
        Just xs -> mfold . fmap mergeConstraints . dnf $ Conjunction xs
      updateConstraints $ toList cs
      -- TODO: how do we deal with running out of fuel? Can we still have
      -- assertions at some nodes of the computation?
      return y
    _ -> error "Should never happen"

-- TODO: maybe reintroduce some informativeness constraint
informative :: Set Hole -> Constraints -> Bool
informative hs cs = hs == Map.keysSet cs

updateConstraints :: [Constraints] -> Synth ()
updateConstraints xs = do
  let ys = nubOrd xs
  guard . not . null $ ys
  assign constraints $ Disjunction . fmap Pure $ ys

-- Experimental {{{

newtype Space a = Space (Map Hole [(a, Space a)])
  deriving (Eq, Ord, Show)
  deriving (Functor, Foldable, Traversable)

instance Pretty a => Pretty (Space a) where
  pretty (Space xs) = Pretty.vsep $ Map.assocs xs <&> \(h, es) ->
    Pretty.nest 2 $ Pretty.vsep (pretty h <> ":" :
      (es <&> \(e, sp) -> Pretty.nest 2 (Pretty.vsep [pretty e, pretty sp])))

data Goal = Goal
  { _goalTyp :: Type
  , _goalEnv :: Map (Term Void) Poly
  } deriving (Eq, Ord, Show)

makeLenses ''Goal

instance Subst Goal where
  subst th = over goalTyp (subst th) . over goalEnv (subst th <$>)

data GenSt = GenSt
  { _genCtxs :: Map Hole Goal
  , _genHole :: Fresh Hole
  , _genVar  :: Fresh Var
  , _genFree :: Fresh Free
  } deriving (Eq, Ord, Show)

makeLenses ''GenSt

type Gen = Reader GenSt

-- Instantiates a polymorphic type with fresh variables.
refreshPoly :: Poly -> State (Fresh Free) Type
refreshPoly (Poly as t) = do
  th <- for as \a -> (a,) <$> getFresh
  return $ subst (Var <$> Map.fromList th) t

-- Generates the eta-expansion of a type.
eta :: Type -> State (Fresh Var) (Term Goal)
eta (Args ts u) = do
  xs <- for ts \t -> (,t) <$> getFresh
  return . lams (fst <$> xs) . Hole . Goal u . Map.fromList
    $ fmap (Var *** Mono) xs

-- Applies eta-expanded holes to a term.
expand :: Term Void -> Type -> State (Fresh Var) (Term Goal, Type)
expand x (Args ts u) = do
  xs <- for ts eta
  return (apps (over holes absurd x) xs, u)

-- Gives each hole a name and returns a mapping from hole names to the original
-- holes' contents.
renumber :: Term a -> State (Fresh Hole) (Term Hole, Map Hole a)
renumber x = do
  e <- forOf holes x \a -> (,a) <$> getFresh
  let m = Map.fromList $ toListOf holes e
  return (over holes fst e, m)

gen :: Gen (Space (Term Hole))
gen = do
  cs <- view genCtxs
  xs <- forMap cs \h (Goal t ctx) ->
    for (Map.assocs ctx) \(v, p) -> readerState do
      m        <- zoom genFree $ refreshPoly p
      (x, u)   <- zoom genVar  $ expand v m
      (y, cs') <- zoom genHole $ renumber x
      case unify u t of
        Nothing -> return $ return Nothing
        Just th -> do
          zoom genCtxs do
            modify $ Map.delete h
            modify $ Map.union $ cs' <&> over goalEnv (ctx <>)
            modify (subst th <$>)
          return $ Just . (y,) <$> gen
  return . Space $ catMaybes <$> xs

updSpace :: (Hole -> a -> State r (Maybe b)) -> Space a -> Reader r (Space b)
updSpace f (Space xs) =
  Space <$> forMap xs \h ys -> catMaybes <$> for ys \(y, sp) -> readerState do
    f h y >>= \case
      Nothing -> return $ return Nothing
      Just z -> return $ Just . (z,) <$> updSpace f sp

fillSpace :: Space (Term Hole) -> Reader (Term Hole) (Space (Term Hole))
fillSpace = updSpace \h x -> do
  modify $ fill (Map.singleton h x)
  gets Just

evalSpace :: Space (Term Hole) -> Reader (Scope, Result) (Space Result)
evalSpace = updSpace \h x -> do
  (en, y) <- get
  let r = runReader (resume (Map.singleton h x) y) en
  assign _2 r
  return $ Just r

-- TODO: equivalence pruning

pruneHoles :: Space Result -> Reader Result (Space Result)
pruneHoles = updSpace \h x -> do
  gets blocking >>= \case
    Nothing -> gets Just
    Just (_, h')
      | h == h' -> do
        put x
        gets Just
      | otherwise -> return Nothing

-- TODO: unevaluation pruning

pipeline :: Reader (GenSt, Term Hole, Scope) (Space Result)
pipeline = do
  es <- magnify _1 gen
  e <- view _2
  magnify _3 do
    r <- eval mempty e
    withReader (,r) $ evalSpace es >>= withReader snd . pruneHoles

limit :: Int -> Space a -> Space a
limit 0 (Space xs) = Space $ set each [] xs
limit n (Space xs) = Space $ over (each . each . _2) (limit (n - 1)) xs

-- TODO: enumerate fairly

-- }}}

elimWeight, schemeWeight, recVarWeight :: Dist
elimWeight = 4
schemeWeight = 4
recVarWeight = 2

-- TODO: should we have a weight here? Giving a weight of 1 to
-- constructors and variables makes the benchmarks an order of
-- magnitude slower! The weight of global variables should probably be scaled
-- further compared to constructors and local variables.
refinements :: (Scope, Hole) -> Bool -> HoleCtx -> Synth (Term (Hole, HoleCtx))
refinements (m, h) b (HoleCtx t ctx) = do
  (e', th) <- join $ mfold
    if b then
    [ do
      (v, Poly _ (Args ts _)) <- mfold $ Map.assocs ctx
      guard . Set.notMember v =<< use scrutinized
      when (recVar v m) $ weigh h recVarWeight
      hs <- for ts $ const (fresh @Hole)
      let e = apps (Var v) (Hole <$> hs)
      modifying scrutinized $ Set.insert v
      check ctx e $ Poly [] t
    ] else
    [ do
      (v, Poly _ (Args ts _)) <- mfold $ Map.assocs ctx
      when (recVar v m) $ weigh h recVarWeight
      hs <- for ts $ const (fresh @Hole)
      let e = apps (Var v) (Hole <$> hs)
      check ctx e $ Poly [] t
    -- For concrete types, try the corresponding constructors.
    , case t of
      Apps (Ctr d) _ -> do
        (_, cs) <- mfold . Map.lookup d =<< view dataTypes
        (c, ts) <- mfold cs
        hs <- for ts $ const fresh
        let e = apps (Ctr c) (Hole <$> hs)
        check ctx e $ Poly [] t
      _ -> mzero
    -- Global variables are handled like local variables, but only if they are
    -- explicitly imported.
    , do
      (v, p@(Poly _ (Args ts _))) <- mfold =<< use included
      hs <- for ts $ const fresh
      -- weigh h 1

      -- Equivalences {{{
      let u = Hole $ Unit ()
      let z = Ctr "Zero"
      let s = App (Ctr "Succ") u
      let n = Ctr "Nil"
      let c = apps (Ctr "Cons") (replicate 2 u)
      case v of
        "plus" | [l, _] <- hs -> do
          let zeros = (`Map.singleton` z) <$> hs
          let succs = (`Map.singleton` s) <$> hs
          let left = Map.singleton l $ apps (Var "plus") (replicate 2 u)
          modifying forbidden (<> (left : zeros ++ succs))
        "mult" | [l, _] <- hs -> do
          let zeros = (`Map.singleton` z) <$> hs
          -- TODO: how to disallow multiplying by 1? We probably need some
          -- more sophisticated representation of forbidden fillings.
          -- let succs = (`Map.singleton` App (Ctr "Succ") u) <$> hs
          let left = Map.singleton l $ apps (Var "mult") (replicate 2 u)
          modifying forbidden (<> (left : zeros))
        "append" | [l, _] <- hs -> do
          let nils = (`Map.singleton` n) <$> hs
          let cons = Map.singleton l c
          let left = Map.singleton l $ apps (Var "append") (replicate 2 u)
          modifying forbidden (<> (left : cons : nils))
        "snoc" | [l, _] <- hs -> do
          let nil = Map.singleton l n
          let cons = Map.singleton l c
          modifying forbidden (<> [nil, cons])
        "even" | [l] <- hs -> do
          let zero = Map.singleton l z
          let succ = Map.singleton l s
          modifying forbidden (<> [zero, succ])
        "length" | [l] <- hs -> do
          let nil = Map.singleton l n
          let cons = Map.singleton l c
          modifying forbidden (<> [nil, cons])
        "sum" | [l] <- hs -> do
          let nil = Map.singleton l n
          let cons = Map.singleton l c
          modifying forbidden (<> [nil, cons])
        "sumrec" | [l] <- hs -> do
          let nil = Map.singleton l n
          let cons = Map.singleton l c
          modifying forbidden (<> [nil, cons])
        "elimTree" | [_, _, l] <- hs -> do
          let leaf = Map.singleton l $ Ctr "Leaf"
          let node = Map.singleton l $ apps (Ctr "Node") (replicate 3 u)
          modifying forbidden (<> [leaf, node])
          modifying varOnly $ Set.insert l
          weigh h elimWeight
        "foldTree" | [_, _, l] <- hs -> do
          let leaf = Map.singleton l $ Ctr "Leaf"
          let node = Map.singleton l $ apps (Ctr "Node") (replicate 3 u)
          modifying forbidden (<> [leaf, node])
          modifying varOnly $ Set.insert l
          weigh h schemeWeight
        "mapTree" | [_, l] <- hs -> do
          let leaf = Map.singleton l $ Ctr "Leaf"
          let node = Map.singleton l $ apps (Ctr "Node") (replicate 3 u)
          let fuse = Map.singleton l $ apps (Var "mapTree") (replicate 2 u)
          modifying forbidden (<> [leaf, node, fuse])
          modifying varOnly $ Set.insert l
        "elimList" | [_, _, l] <- hs -> do
          let nil = Map.singleton l n
          let cons = Map.singleton l c
          modifying forbidden (<> [nil, cons])
          modifying multiplier $ Map.insert l 2
          weigh h elimWeight
        "foldList" | _:_:l:_ <- hs -> do
          let nil = Map.singleton l n
          let cons = Map.singleton l c
          modifying forbidden (<> [nil, cons])
          modifying varOnly $ Set.insert l
          weigh h schemeWeight
        "foldr" | _:_:l:_ <- hs -> do
          let nil = Map.singleton l n
          let cons = Map.singleton l c
          modifying forbidden (<> [nil, cons])
          modifying varOnly $ Set.insert l
          weigh h schemeWeight
        "foldl" | _:_:l:_ <- hs -> do
          let nil = Map.singleton l n
          let cons = Map.singleton l c
          modifying forbidden (<> [nil, cons])
          modifying varOnly $ Set.insert l
          weigh h schemeWeight
        "paraList" | [_, _, l] <- hs -> do
          let nil = Map.singleton l n
          let cons = Map.singleton l c
          modifying forbidden (<> [nil, cons])
          modifying varOnly $ Set.insert l
          weigh h schemeWeight
        "map" | [_, l] <- hs -> do
          let nil = Map.singleton l n
          let cons = Map.singleton l c
          let fuse = Map.singleton l $ apps (Var "map") (replicate 2 u)
          modifying forbidden (<> [nil, cons, fuse])
        "concat" | [l] <- hs -> do
          let nil = Map.singleton l n
          modifying forbidden (<> [nil])
        "concatMap" | [f, l] <- hs -> do
          let empty = Map.singleton f n
          let nil = Map.singleton l n
          let cons = Map.singleton l c
          modifying forbidden (<> [empty, nil, cons])
        "zipWith" | [_, l, r] <- hs -> do
          let left = Map.singleton l c
          let right = Map.singleton r c
          let fuse = Map.singleton l $ apps (Var "zipWith") (replicate 3 u)
          modifying forbidden (<> [left, right, fuse])
        "elimNat" | [_, _, l] <- hs -> do
          let zero = Map.singleton l z
          let succ = Map.singleton l s
          modifying forbidden (<> [zero, succ])
          modifying multiplier $ Map.insert l 2
          weigh h elimWeight
        "foldrNat" | _:_:l:_ <- hs -> do
          let zero = Map.singleton l z
          let succ = Map.singleton l s
          modifying forbidden (<> [zero, succ])
          modifying varOnly $ Set.insert l
          weigh h schemeWeight
        "elimBool" | [_, _, l] <- hs -> do
          let false = Map.singleton l $ Ctr "False"
          let true = Map.singleton l $ Ctr "True"
          modifying forbidden (<> [false, true])
          modifying multiplier $ Map.insert l 2
          weigh h elimWeight
        "elimOrd" | [_, _, _, l] <- hs -> do
          let lt = Map.singleton l $ Ctr "LT"
          let eq = Map.singleton l $ Ctr "EQ"
          let gt = Map.singleton l $ Ctr "GT"
          modifying forbidden (<> [lt, eq, gt])
          modifying multiplier $ Map.insert l 2
          weigh h elimWeight
        "compareNat" | [x, y] <- hs -> do
          let zz = Map.fromList [(x, z), (y, z)]
          let zs = Map.fromList [(x, z), (y, s)]
          let sz = Map.fromList [(x, s), (y, z)]
          let ss = Map.fromList [(x, s), (y, s)]
          -- NOTE: since eq is commutative, we can disallow one Succ/Zero,
          -- since two is already forbidden
          let zero = Map.singleton x z
          let succ = Map.singleton x s
          modifying forbidden (<> [zero, succ, zz, zs, sz, ss])
        "eq" | [x, y] <- hs -> do
          let zz = Map.fromList [(x, z), (y, z)]
          let zs = Map.fromList [(x, z), (y, s)]
          let sz = Map.fromList [(x, s), (y, z)]
          let ss = Map.fromList [(x, s), (y, s)]
          -- NOTE: since eq is commutative, we can disallow one Succ/Zero,
          -- since two is already forbidden
          let zero = Map.singleton x z
          let succ = Map.singleton x s
          modifying forbidden (<> [zero, succ, zz, zs, sz, ss])
        "neq" | [x, y] <- hs -> do
          let zz = Map.fromList [(x, z), (y, z)]
          let zs = Map.fromList [(x, z), (y, s)]
          let sz = Map.fromList [(x, s), (y, z)]
          let ss = Map.fromList [(x, s), (y, s)]
          -- NOTE: since neq is commutative, we can disallow one Succ/Zero,
          -- since two is already forbidden
          let zero = Map.singleton x z
          let succ = Map.singleton x s
          modifying forbidden (<> [zero, succ, zz, zs, sz, ss])
        "max" | [x, y] <- hs -> do
          let zeroes = (`Map.singleton` z) <$> hs
          -- NOTE: since max is commutative, we can disallow one Succ, since
          -- two is already forbidden
          let succ = Map.singleton x s
          let ss = Map.fromList [(x, s), (y, s)]
          let left = Map.singleton x $ apps (Var "max") (replicate 2 u)
          modifying forbidden (<> (left:succ:ss:zeroes))
        "leq" | [x, y] <- hs -> do
          let zero = Map.singleton x z
          let succs = Map.fromList [(x, s), (y, s)]
          modifying forbidden (<> [zero, succs])
        "any" | [f, l] <- hs -> do
          let false = Map.singleton f $ Ctr "False"
          let true = Map.singleton f $ Ctr "True"
          let nil = Map.singleton l n
          modifying forbidden (<> [false, true, nil])
        "or" | [_, _] <- hs -> do
          let falses = (`Map.singleton` Ctr "False") <$> hs
          let trues = (`Map.singleton` Ctr "True") <$> hs
          modifying forbidden (<> falses <> trues)
        "elem" | [_, l] <- hs -> do
          let nil = Map.singleton l n
          let cons = Map.singleton l c
          modifying forbidden (<> [nil, cons])
        "elimMoney" | [_, _, _, l] <- hs -> do
          let one = Map.singleton l $ Ctr "One"
          let two = Map.singleton l $ Ctr "Two"
          let chk = Map.singleton l $ App (Ctr "Check") u
          modifying forbidden (<> [one, two, chk])
          weigh h elimWeight
        _ -> return ()
      -- }}}

      let e = apps (Var v) (Hole <$> hs)

      -- NOTE: this local is only needed to add the function with instantiated
      -- types to the environment to check. It would be more efficient to not
      -- call check, but simply do the type checking manually.
      let help = set functions $ Map.singleton v p
      local help $ check ctx e $ Poly [] t
    ]
  let result = strip e'
  let hs = toListOf (holes . _1) result
  weigh h $ fromIntegral $ length hs
  let ts = toListOf (holes . _2 . goalType) result
  weigh h $ fromIntegral $
    sum (map (\u -> max 0 (typeSize u - typeSize t)) ts)
  n <- fromMaybe 1 . Map.lookup h <$> use multiplier
  modifying multiplier $ Map.mapWithKey \k d ->
    if k `elem` hs then n * d else d
  modifying contexts (subst th <$>)
  return result

removeMatching :: (Ord k, Eq v) => k -> v -> Map k v -> Maybe (Map k v)
removeMatching k v m = case Map.lookup k m of
  Nothing -> return m
  Just x -> do
    guard $ x == v
    return $ Map.delete k m

updateForbidden :: Hole -> Term Unit -> Synth ()
updateForbidden h e = do
  fs <- use forbidden
  let fs' = mapMaybe (removeMatching h e) fs
  guard $ not . any null $ fs'
  assign forbidden fs'

unevalFuel :: Int
unevalFuel = 32

-- TODO: find a good amount of disjunctions allowed.
maxDisjunctions :: Int
maxDisjunctions = 32

step :: Map Hole (Term Hole) -> Synth ()
step hf = do
  -- Pick one blocking hole and remove it.
  s <- use mainScope
  rs <- use examples >>= mapM (liftEval . evalAssert s)
  -- TODO: if there are multiple blocking holes, we could compute hole
  -- dependencies and base the order on that
  (m, hole) <- mfold $ foldr (<|>) Nothing (blocking . fst <$> rs)
  ctx <- mfold . Map.lookup hole =<< use contexts
  modifying contexts $ Map.delete hole
  -- Find a refinement to extend the hole filling.
  var <- Set.member hole <$> use varOnly
  modifying varOnly $ Set.delete hole
  ref <- refinements (m, hole) var ctx
  updateForbidden hole $ over holes (const $ Unit ()) ref
  expr <- etaExpand _2 ref
  modifying contexts (<> Map.fromList (toListOf holes expr))
  let new = Map.singleton hole $ over holes fst expr
  use mainScope
    >>= mapM (liftEval . resume new)
    >>= assign mainScope
  modifying fillings (<> new)
  let hf' = hf <> new
  -- NOTE: we eagerly fill in scrutinized holes. Does that make sense? In some
  -- cases it might be better to unevaluate, so that we know better how to fill
  -- the scrutinized hole... TODO: find some good examples
  scrHole <- scrutinizedHole <$> liftEval do
    eval m (over holes fst expr) >>= resume new
  case scrHole of
    Just _h -> do
      step hf'
    Nothing -> do
      cs <- use constraints
      let noUneval = False
      ctxs <- use contexts
      if noUneval && not (Map.null ctxs)
        then do
          step hf'
        else do
          liftUneval unevalFuel (resumeUneval hf' cs) >>= \case
            Nothing -> do
              weigh hole 1
              step hf'
            Just xs -> do
              let disjunctions = dnf xs
              if length disjunctions > maxDisjunctions
                then do
                  -- NOTE: weights are added a bit too late. we add a weight
                  -- for e.g. using a recursively defined variable, but that's
                  -- because unevaluation takes so long, which will happen
                  -- anyways, because the weight only affects hole fillings
                  -- after that!
                  weigh hole 4
                  step hf'
                else updateConstraints $ disjunctions >>= toList . mergeConstraints

-- TODO: add a testsuite testing the equivalence of different kinds and
-- combinations of (un)evaluation resumptions.

final :: SynState -> Bool
final = null . view contexts

synth :: Defs Unit -> Synth (Map Hole (Term Hole))
synth d = init d >> go where
  go :: Synth (Map Hole (Term Hole))
  go = do
    st <- get
    if final st
      then use fillings
      else step mempty >> go
