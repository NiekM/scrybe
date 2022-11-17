{-# LANGUAGE TemplateHaskell #-}

module Synthesis where

import Import
import qualified Prettyprinter as Pretty
import Options (SynOptions(..), synPropagate)
import Utils.Weighted
import Language
import qualified RIO.Map as Map
import qualified RIO.Set as Set

-- Synthesis Monad {{{

type Fillings = Map Hole (Term Hole)

data SynState = SynState
  { _options     :: SynOptions
  , _contexts    :: Map Hole Goal
  , _constraints :: Logic Constraints
  , _examples    :: [Assert]
  , _included    :: [(Var, Poly)]
  , _forbidden   :: [Map Hole (Term Unit)]
  , _mainScope   :: Scope
  , _fillings    :: Fillings
  -- Fresh variables
  , _freshHole   :: Fresh Hole
  , _freshFree   :: Fresh Free
  , _freshVar    :: Fresh Var
  -- Heuristics
  , _varCount    :: Sum Int
  }

makeLenses ''SynState

mkSynState :: SynOptions -> SynState
mkSynState opts = SynState opts mempty (Disjunction []) mempty mempty mempty
  mempty mempty mempty mempty mempty mempty

type Nondet = Search Dist
type Synth = RWST Env () SynState Nondet

runSynth :: SynOptions -> Env -> Synth a -> Nondet a
runSynth opts m x = view _1 <$> runRWST x m (mkSynState opts)

-- }}}

liftInfer :: Infer a -> Synth a
liftInfer = mapRWST mfold . zoom freshFree

liftUneval :: Int -> Uneval a -> Synth (Maybe a)
liftUneval fuel x = ask <&> \e -> view _1 <$> runRWST x e fuel

checkBindings :: [Signature] -> [Binding Unit] -> Infer (Term Goal)
checkBindings ss fs = do
  let sigs = Map.fromList $ ss <&> \(MkSignature a (Poly _ t)) -> (a, t)
  let e = Lets (fs <&> \(MkBinding a x) -> (a, x)) (Hole Unit)
  (_, a) <- infer mempty e
  case a of
    x@(Lets _ (Hole (Goal cs _))) -> do
      let funs = cs <&> \(Poly _ t) -> t
      th <- failMaybe . unifies $ Map.intersectionWith (,) funs sigs
      return $ over holes
        ( over goalType freezeAll
        . over goalCtx (freezeUnbound . instantiate th <$>)
        . subst th
        ) x
    _ -> error "Impossible"

init :: Defs Unit -> Synth (Term Hole)
init defs = do
  let ws = [x | Include xs <- pragmas defs, x <- toList xs]

  gs <- asks $ view envFunctions
  ws' <- zoom freshFree $ forOf (each . _2 . each) ws refresh

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

  -- Type check
  expr <- liftInfer $ checkBindings (signatures defs) (bindings defs)
  -- Eta expand
  expanded <- zoom freshVar $ expand expr
  -- Name holes and extract context
  (e, ctx) <- zoom freshHole $ extract expanded
  assign contexts ctx

  liftEval (eval mempty e) >>= \case
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
      return e
    _ -> error "Should never happen"

-- TODO: maybe reintroduce some informativeness constraint
informative :: Set Hole -> Constraints -> Bool
informative hs cs = hs == Map.keysSet cs

updateConstraints :: [Constraints] -> Synth ()
updateConstraints xs = do
  let ys = nubOrd xs
  guard . not . null $ ys
  assign constraints $ Disjunction . fmap Pure $ ys

recVarWeight :: Dist
recVarWeight = 2

addVar :: Synth ()
addVar = do
  Sum n <- use varCount
  weigh @Dist $ fromIntegral n
  modifying varCount (+1)

match :: Term Hole -> Term Unit -> Maybe (Map Hole (Term Unit))
match = curry \case
  (_, Hole _) -> Just mempty
  (Hole h, e) -> Just $ Map.singleton h e
  (Var x, Var y) | x == y -> Just mempty
  (Ctr c, Ctr d) | c == d -> Just mempty
  (App f x, App g y) -> liftM2 (<>) (match f g) (match x y)
  (Lam a x, Lam b y) -> match x (replace (Map.singleton b $ Var a) y)
  _ -> Nothing

refinements :: (Scope, Hole) -> Goal -> Synth (Term Hole)
refinements (m, h) (Goal ctx t) = do
  (e, ts) <- join $ mfold
    -- Local variables
    [ do
      (v, Poly _ (Args ts _)) <- mfold $ Map.assocs ctx
      when (recVar v m) do
        weigh recVarWeight
      return (Var v, ts)
    -- Constructors
    , case t of
      Apps (Ctr d) _ -> do
        (_, cs) <- mfold . Map.lookup d =<< view envDatatypes
        (c, ts) <- mfold cs
        return (Ctr c, ts)
      _ -> mzero
    -- Imported functions
    , do
      (v, Poly _ (Args ts _)) <- mfold =<< use included
      return (Var v, ts)
    ]
  (_, e1, th) <- liftInfer . check ctx (apps e (Hole <$> ts)) $ Poly [] t
  modifying contexts (subst th <$>)
  e2         <- zoom freshVar  $ expand e1
  (e3, ctx') <- zoom freshHole $ extract e2
  modifying contexts (<> ctx')

  -- Weigh 1 for each new hole
  weigh @Dist $ fromIntegral $ length ts
  -- Each new variable weighs more
  for_ ts \(Args as _) -> for_ as $ const addVar

  -- Forbidden
  updateForbidden h $ over holes (const Unit) e1
  fb <- mapMaybe (match e3) . view envForbidden <$> ask
  modifying forbidden (<> fb)

  return e3

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

step :: Fillings -> Synth ()
step hf = do
  -- Pick one blocking hole and remove it.
  s <- use mainScope
  rs <- use examples >>= mapM (liftEval . evalAssert s)
  -- TODO: if there are multiple blocking holes, we could compute hole
  -- dependencies and base the order on that
  (m, hole) <- mfold $ foldr (<|>) Nothing (blocking . fst <$> rs)
  ctx <- mfold . Map.lookup hole =<< use contexts
  modifying contexts $ Map.delete hole
  -- Find a possible refinement.
  expr <- refinements (m, hole) ctx
  -- Update hole fillings.
  let new = Map.singleton hole expr
  use mainScope
    >>= mapM (liftEval . resume new)
    >>= assign mainScope
  modifying fillings (<> new)
  let hf' = hf <> new
  -- NOTE: we eagerly fill in scrutinized holes. Does that make sense? In some
  -- cases it might be better to unevaluate, so that we know better how to fill
  -- the scrutinized hole... TODO: find some good examples
  scrHole <- scrutinizedHole <$> liftEval do
    eval m expr >>= resume new

  cs <- use constraints
  ctxs <- use contexts
  propagate <- use $ options . synPropagate

  case scrHole of
    Just _h -> step hf'
    Nothing
      | not propagate && not (Map.null ctxs) -> step hf'
      | otherwise -> do
        liftUneval unevalFuel (resumeUneval hf' cs) >>= \case
          Nothing -> do
            weigh @Dist 1
            step hf'
          Just xs
            | length disjunctions > maxDisjunctions -> do
              -- NOTE: weights are added a bit too late. we add a weight
              -- for e.g. using a recursively defined variable, but that's
              -- because unevaluation takes so long, which will happen
              -- anyways, because the weight only affects hole fillings
              -- after that!
              weigh @Dist 4
              step hf'
            | otherwise ->
              updateConstraints $ disjunctions >>= toList . mergeConstraints
            where disjunctions = dnf xs

final :: SynState -> Bool
final = null . view contexts

synth :: Defs Unit -> Synth Fillings
synth d = init d >> go where
  go :: Synth Fillings
  go = do
    st <- get
    if final st
      then use fillings
      else step mempty >> go

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

-- TODO: add a testsuite testing the equivalence of different kinds and
-- combinations of (un)evaluation resumptions.

-- EXPERIMENTAL {{{

newtype Space n a = Space (Map Hole [(a, n, Space n a)])
  deriving (Eq, Ord, Show)
  deriving (Functor, Foldable, Traversable)

instance Pretty a => Pretty (Space n a) where
  pretty (Space xs) = Pretty.vsep $ Map.assocs xs <&> \(h, es) ->
    Pretty.nest 2 $ Pretty.vsep (pretty h <> ":" :
      (es <&> \(e, _, sp) -> Pretty.nest 2 (Pretty.vsep [pretty e, pretty sp])))

data GenSt = GenSt
  { _genCtxs :: Map Hole Goal
  , _genFuns :: Map Var Poly
  , _genHole :: Fresh Hole
  , _genVar  :: Fresh Var
  , _genFree :: Fresh Free
  } deriving (Eq, Ord, Show)

makeLenses ''GenSt

mkGenSt :: GenSt
mkGenSt = GenSt mempty mempty mempty mempty mempty

data Info = Info
  { _weight :: Dist
  }

makeLenses ''Info

type Gen = Reader GenSt

generate :: Env -> Map Var Poly -> Map Hole Goal -> Space Info (Term Hole)
generate env fs gs = runReader (gen env) $ GenSt gs fs 1 0 0

init_ :: Defs Unit -> RWST Env () GenSt Maybe (Term Hole)
init_ defs = do
  checked   <- zoom genFree $ checkBindings (signatures defs) (bindings defs)
  expanded  <- zoom genVar  $ expand checked
  (e, ctxs) <- zoom genHole $ extract expanded
  assign genCtxs ctxs
  let ws = [x | Include xs <- pragmas defs, x <- toList xs]
  ws' <- zoom genFree $ forOf (each . _2 . each) ws refresh
  gs <- asks $ view envFunctions
  assign genFuns . Map.fromList $ ws' <&> \(v, a) ->
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
  case e of
    Lets _ (Hole h) -> do
      modifying genCtxs $ Map.delete h
      return e
    _ -> error "Something went wrong"

withScope :: Scope -> Space n (Term Hole) -> Space n Scope
withScope s (Space xs) = Space $ xs & Map.mapWithKey \h -> map \(e, n, ys) ->
  let s' = runReader (for s $ resume (Map.singleton h e)) mempty
  in (s', n, withScope s' ys)

expl :: Space Info a -> Nondet a
expl (Space xs) = do
  (_, ys) <- mfold $ Map.assocs xs
  (x, i, zs) <- mfold ys
  weigh $ view weight i
  case zs of
    Space z
      | null z    -> return x
      | otherwise -> expl zs

refs :: Env -> Map Var Poly -> Goal -> [(Term Void, Poly)]
refs env funs (Goal ctx t) = join
  [ case t of
      Apps (Ctr d) _
        | Just (as, cs) <- Map.lookup d (view envDatatypes env) -> do
        (c, ts) <- cs
        return (Ctr c, Poly as . arrs $ ts ++ [apps (Ctr d) (Var <$> as)])
      _ -> []
  , Map.assocs ctx  <&> first Var
  , Map.assocs funs <&> first Var
  ]

gen :: Env -> Gen (Space Info (Term Hole))
gen env = do
  cs <- view genCtxs
  fs <- view genFuns
  Space <$> forMap cs \h g@(Goal ctx t) -> catMaybes <$>
    for (refs env fs g) \(f, p) -> readerStateMaybe do
      Args ts u <- zoom genFree $ instantiateFresh p
      Just th   <- return $ unify u t
      let e = apps (magic f) (Hole . Goal ctx <$> ts)
      x         <- zoom genVar  $ expand e
      (y, cs')  <- zoom genHole $ extract x
      zoom genCtxs do
        modify $ Map.delete h
        modify $ Map.union cs'
        modify (subst th <$>)
      -- let n = 1 + sum (ts <&> \(Args as _) -> 1 + length as)
      let n = 1 + sum (typeSize <$> ts)
      return $ (y,Info $ fromIntegral n,) <$> gen env

updForbidden :: Hole -> Term Hole -> [Map Hole (Term Unit)] -> Maybe [Map Hole (Term Unit)]
updForbidden h e = sequence . mapMaybe \old -> case Map.lookup h old of
  Nothing -> Just (Just old)
  Just x ->
    let upd = Map.delete h old
    in case match e x of
      Nothing -> Nothing
      Just new
        | null (upd <> new) -> Just Nothing
        | otherwise -> Just $ Just (upd <> new)

-- TODO: forbid could be implemented nicely using uniplate, something like:
-- everywhere forbid'
forbid :: [Term Unit] -> Space n (Term Hole) -> Space n (Term Hole)
forbid es (Space xs) = forbid' (Map.keys xs >>= \h -> Map.singleton h <$> es)
  . Space $ over (each . each . _3) (forbid es) xs

forbid' :: [Map Hole (Term Unit)] -> Space n (Term Hole) -> Space n (Term Hole)
forbid' fs (Space xs)
  | null fs = Space xs
  | otherwise = Space $ xs & Map.mapWithKey \h -> mapMaybe \(e, n, z) ->
    case updForbidden h e fs of
      Nothing -> Nothing
      Just fs' -> Just (e, n, forbid' fs' z)

findCounterExample :: Scope -> [Assert] -> Eval (Maybe Assert)
findCounterExample s as = do
  bs <- for as \a -> do
    (r, x) <- evalAssert s a
    return case downcast r of
      Nothing -> error "Something went wrong in evaluation"
      Just v
        | consistent v x -> Nothing
        | otherwise -> Just a
  return . mfold $ catMaybes bs

pruneHoles :: Env -> Scope -> Term Hole -> Space n Scope -> Space n Scope
pruneHoles env s e (Space xs) = Space case blocking r of
  Nothing -> xs
  Just (_, h) -> Map.restrictKeys xs (Set.singleton h) <&>
    map \(s', n, xs') -> (s', n, pruneHoles env s' e xs')
  where
    r :: Result
    r = runReader (eval s e) $ view envScope env

pruneExample :: Env -> Assert -> Space n Scope -> Space n Scope
pruneExample env a@(MkAssert e ex) (Space xs) = Space $ xs <&> mapMaybe
  \(s, n, ys) ->
  let
    r :: Result
    r = runReader (eval s e) $ view envScope env
    l :: Maybe (Logic Constraints)
    l = runUneval env 100 $ uneval r $ toEx ex
  in case l of
    Nothing ->
      -- traceShow ("Out of fuel: " <> pretty s) $
      Just (s, n, pruneExample env a ys)
    Just cs -> case mergeConstraints <$> dnf cs of
      [] ->
        -- traceShow ("Structural conflict:" Pretty.<+> pretty s)
        Nothing
      ds
        | null (catMaybes ds) ->
          -- trace "Inconsistency"
          Nothing
        | otherwise -> Just (s, n, pruneExample env a ys)

trim :: Int -> Space n a -> Space n a
trim 0 (Space _) = Space mempty
trim n (Space xs) = Space $ xs <&> over (each . _3) (trim $ n - 1)

measure :: Int -> Space n a -> Int
measure n = length . trim n

-- }}}
