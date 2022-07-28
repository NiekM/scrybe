{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}

module Synthesis where

import Import
import Language
import qualified RIO.Map as Map
import Control.Monad.Heap
import Control.Monad.State
import Control.Monad.RWS
import Data.Monus.Dist

-- TODO: maybe we also want to keep track of a list of possible refinements
-- e.g. [Term Unit]
data SynState = SynState
  { _contexts    :: Map Hole HoleCtx
  , _constraints :: Logic Constraints
  , _fillings    :: Map Hole (Term Hole)
  , _freshSt     :: FreshState
  , _mainScope   :: Scope
  , _examples    :: [Assert]
  , _included    :: [(Var, Poly, Int)]
  , _forbidden   :: [Map Hole (Term Unit)]
  }

makeLenses ''SynState

emptySynState :: SynState
emptySynState =
  SynState mempty (Disjunction []) mempty mkFreshState mempty [] mempty []

runSynth :: Env -> Synth a -> Heap Dist a
runSynth m x = fst <$> runReaderT (runStateT x emptySynState) m

instance HasFreshState SynState where
  freshState = freshSt

type Synth = StateT SynState (ReaderT Env (Heap Dist))

instance LiftEval Synth where
  liftEval x = runIdentity . runReaderT x <$> view env

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

-- TODO: perhaps Nondet should be a wrapper around Heap providing this
-- MonadFail instance.
instance MonadFail (Heap w) where
  fail _ = mzero

init :: Defs Unit -> Synth (Term Hole)
init defs = do
  -- let ws = concat $ imports defs <&> \(MkImport _ xs) -> fromMaybe [] xs
  let ws = [x | Include xs <- pragmas defs, x <- toList xs]
  -- TODO: determine weights based on something

  gs <- view functions <$> ask
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
    , case v of
        "foldTree" -> 6
        "elimNat" -> 3
        "foldNat" | Just _ <- a -> 10
        "foldList" | Just _ <- a -> 10
        _ -> 1
    )

  let addBinding (MkBinding a x) y = Let a x y
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
  -- traceShowM . pretty $ y
  liftEval (eval mempty y) >>= \case
    Scoped m (Hole h) -> do
      assign mainScope m
      modifying contexts $ Map.delete h
      assign examples $ asserts defs
      -- TODO: find reasonable fuel
      cs <- liftUneval 1000 (for (asserts defs) (unevalAssert m)) >>= \case
        Nothing -> fail "Out of fuel"
        Just xs -> mfold . fmap mergeConstraints . dnf $ Conjunction xs
      updateConstraints cs
      -- TODO: how do we deal with running out of fuel? Can we still have
      -- assertions at some nodes of the computation?
      return y
    _ -> error "Should never happen"

-- resHoles :: Result -> Set Hole
-- resHoles = cataExpr \case
--   Scoped m (Hole h) -> Set.singleton h <> scopeHoles m
--   Scoped m _ -> scopeHoles m
--   App f x -> f <> x
--   _ -> mempty

-- scopeHoles :: Scope -> Set Hole
-- scopeHoles = foldMap resHoles

-- constrained :: Constraints -> Set Hole
-- constrained cs = Map.keysSet cs
--   <> let
--     bar = foldMap Map.keys $ toList cs
--     baz = foldMap scopeHoles bar
--   in baz

-- TODO: maybe reintroduce some informativeness constraint
informative :: Set Hole -> Constraints -> Bool
informative hs cs = hs == Map.keysSet cs
-- informative hs cs = hs == constrained cs

updateConstraints :: [Constraints] -> Synth ()
updateConstraints xs = do
  let ys = nubOrd xs
  guard . not . null $ ys
  assign constraints $ Disjunction . fmap Pure $ ys

-- updateConstraints :: [Constraints] -> Synth ()
-- updateConstraints xs = do
--   let ys = nubOrd xs
--   hs <- Map.keysSet <$> use contexts
--   -- Make sure every hole has constraints. ('Informativeness restriction')
--   let zs = filter (informative hs) ys
--   guard . not . null $ zs
--   assign constraints $ Disjunction . fmap Pure $ zs

-- TODO: sometimes it's faster to introduce helper functions rather than
-- eliminators/folds (e.g. introducing `not` in `nat_even`), but other times
-- it is better to just intoduce the fold (e.g. introducing `append` in
-- `tree_collect`)

-- TODO: should we have a weight here? Giving a weight of 1 to
-- constructors and variables makes the benchmarks an order of
-- magnitude slower! The weight of global variables should probably be scaled
-- further compared to constructors and local variables.
refinements :: HoleCtx -> Synth (Term (Hole, HoleCtx))
refinements (HoleCtx t ctx) = do
  (e', _) <- join $ mfold
    -- TODO: make sure that recursive functions do not loop infinitely.
    -- For local variables, introduce a hole for every argument.
    [ do
      (v, Poly _ (Args ts _)) <- mfold $ Map.assocs ctx
      hs <- for ts $ const (fresh @Hole)
      tell $ fromIntegral $ length ts
      let e = apps (Var v) (Hole <$> hs)
      let help = set functions mempty
      local help $ check ctx e $ Poly [] t
    -- For concrete types, try the corresponding constructors.
    -- TODO: check if constructors are introduced correctly, see list_map.hs
    -- NOTE: giving constructors a higher weight makes list_sort way faster,
    -- but list_rev_fold seems to diverge.
    , case t of
      Apps (Ctr d) _ -> do
        (_, cs) <- mfold . Map.lookup d =<< view dataTypes
        (c, ts) <- mfold cs
        hs <- for ts $ const fresh
        let e = apps (Ctr c) (Hole <$> hs)
        let help = set functions mempty
        local help $ check ctx e $ Poly [] t
      _ -> mzero
    -- Global variables are handled like local variables, but only if they are
    -- explicitly imported.
    , do
      (v, p@(Poly _ (Args ts _)), w) <- mfold =<< use included
      -- TODO: do we update the weights?
      tell $ fromIntegral $ w + length ts
      hs <- for ts $ const fresh

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
        "foldTree" | [_, _, l] <- hs -> do
          let leaf = Map.singleton l $ Ctr "Leaf"
          let node = Map.singleton l $ apps (Ctr "Node") (replicate 3 u)
          modifying forbidden (<> [leaf, node])
        "mapTree" | [_, l] <- hs -> do
          let leaf = Map.singleton l $ Ctr "Leaf"
          let node = Map.singleton l $ apps (Ctr "Node") (replicate 3 u)
          let fuse = Map.singleton l $ apps (Var "mapTree") (replicate 2 u)
          modifying forbidden (<> [leaf, node, fuse])
        "elimList" | [_, _, l] <- hs -> do
          let nil = Map.singleton l n
          let cons = Map.singleton l c
          modifying forbidden (<> [nil, cons])
        "foldList" | _:_:l:_ <- hs -> do
          let nil = Map.singleton l n
          let cons = Map.singleton l c
          modifying forbidden (<> [nil, cons])
        "paraList" | [_, _, l] <- hs -> do
          let nil = Map.singleton l n
          let cons = Map.singleton l c
          modifying forbidden (<> [nil, cons])
        "map" | [_, l] <- hs -> do
          let nil = Map.singleton l n
          let cons = Map.singleton l c
          let fuse = Map.singleton l $ apps (Var "map") (replicate 2 u)
          modifying forbidden (<> [nil, cons, fuse])
        "zipWith" | [_, l, r] <- hs -> do
          let left = Map.singleton l c
          let right = Map.singleton r c
          modifying forbidden (<> [left, right])
        "elimNat" | [_, _, l] <- hs -> do
          let zero = Map.singleton l z
          let succ = Map.singleton l s
          modifying forbidden (<> [zero, succ])
        "foldNat" | _:_:l:_ <- hs -> do
          let zero = Map.singleton l z
          let succ = Map.singleton l s
          modifying forbidden (<> [zero, succ])
        "foldNatIndexed" | [_, _, l, _] <- hs -> do
          let zero = Map.singleton l z
          let succ = Map.singleton l s
          modifying forbidden (<> [zero, succ])
        "elimBool" | [_, _, l] <- hs -> do
          let false = Map.singleton l $ Ctr "False"
          let true = Map.singleton l $ Ctr "True"
          modifying forbidden (<> [false, true])
        "elimOrd" | [_, _, _, l] <- hs -> do
          let lt = Map.singleton l $ Ctr "LT"
          let eq = Map.singleton l $ Ctr "EQ"
          let gt = Map.singleton l $ Ctr "GT"
          modifying forbidden (<> [lt, eq, gt])
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
        _ -> return ()
      -- }}}

      let e = apps (Var v) (Hole <$> hs)
      let help = set functions $ Map.singleton v p
      local help $ check ctx e $ Poly [] t
    ]
  return $ strip e'

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

step :: Map Hole (Term Hole) -> Synth ()
step hf = do
  -- traceShowM . pretty =<< use fillings
  -- Pick one blocking hole and remove it.
  s <- use mainScope
  rs <- use examples >>= mapM (liftEval . evalAssert s)
  (m, hole) <- mfold $ foldr (<|>) Nothing (blocking . fst <$> rs)
  ctx <- mfold . Map.lookup hole =<< use contexts
  modifying contexts $ Map.delete hole
  -- Find a refinement to extend the hole filling.
  ref <- refinements ctx
  updateForbidden hole $ over holes (const $ Unit ()) ref
  expr <- etaExpand _2 ref
  modifying contexts (<> Map.fromList (toListOf holes expr))
  let new = Map.singleton hole $ over holes fst expr
  use mainScope
    >>= mapM (liftEval . resume new)
    >>= assign mainScope
  modifying fillings (<> new)
  let hf' = hf <> new
  recHole <- recursive <$> liftEval do
    eval m (over holes fst expr) >>= resume new
  case recHole of
    Just _h -> do
      -- traceShowM $ "Recursive position: " <> show _h
      tell 1
      step hf'
    Nothing -> do
      cs <- use constraints
      liftUneval 32 (resumeUneval hf' cs) >>= \case
        Nothing -> do
          -- traceM "Out of fuel"
          tell 1
          step hf'
        Just xs -> do
          let disjunctions = dnf xs
          -- TODO: find a good amount of disjunctions allowed.
          if length disjunctions > 32
            then do
              -- traceM $ "Too many disjunctions: "
              --   <> (fromString . show . length $ disjunctions)
              tell 1
              step hf'
            else updateConstraints $ disjunctions >>= mergeConstraints

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
