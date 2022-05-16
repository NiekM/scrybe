{-# LANGUAGE TemplateHaskell#-}

module Synthesis where

import Import
import Language
import qualified RIO.Map as Map
import qualified RIO.Set as Set

type RefMonad s m = (TCMonad m, FreshVar m, MonadState s m, HasCtxs s)

---- TODO: does init make sense? Maybe we should just have a module as input
---- and compute the GenState
init :: RefMonad s m => Sketch -> m (Term Hole)
init (Sketch _ t e) = do
  (expr, _, ctx) <- check' e t
  assign holeCtxs ctx
  etaExpand $ strip expr

data SynState = SynState
  { _typeCtx :: Map Hole HoleCtx
  , _evalCtx :: Map Hole Constraint
  , _filled  :: Map Hole (Term Hole)
  , _freshSt :: FreshState
  }

makeLenses ''SynState

instance HasCtxs SynState where
  holeCtxs = typeCtx
instance HasFreshState SynState where
  freshState = freshSt

type SynMonad m = (FreshVar m, FreshHole m,
  MonadState SynState m, MonadReader Mod m, MonadPlus m)

step :: SynMonad m => m (Hole, Term Hole)
step = do
  -- Pick one hole and remove it. TODO: not just pick the first hole.
  ((hole, HoleCtx t env), _) <- mfold . Map.minViewWithKey =<< use typeCtx
  modifying typeCtx $ Map.delete hole
  (hole,) <$> case t of
    -- For concrete types, try the corresponding constructors.
    Apps (Ctr d) ts -> do
      (as, cs) <- mfold . Map.lookup d . data_ =<< ask
      -- Pick a constructor.
      (c, us) <- mfold cs
      let th = Map.fromList $ zip as ts
      let ctxs = us <&> \u -> HoleCtx (subst th u) env
      -- Eta expand expression and compute new type contexts.
      expr <- etaExpand (apps (Ctr c) (Hole <$> ctxs))
        >>= traverseOf holes \ctx -> (,ctx) <$> fresh
      modifying typeCtx (<> Map.fromList (toListOf holes expr))
      let expr' = over holes fst expr
      -- Resume unevaluation by refining with a constructor applied to holes.
      assign evalCtx =<< resumeUneval hole expr' =<< use evalCtx
      -- If unevaluation resumption succeeds, return the valid hole filling.
      return expr'
    _ -> mzero


-- OLD {{{
-- Synthesis state {{{

---- TODO: factor out EvalState

--data SynState = SynState
--  { _holeCtxs  :: Map Hole HoleCtx
--  , _env       :: Env
--  , _concepts  :: MultiSet Concept
--  , _fresh     :: FreshState
--  }

--instance HasCtxs SynState where
--  holeCtxs = lens _holeCtxs \x y -> x { _holeCtxs = y }

--instance HasEnv SynState where
--  environment = lens _env \x y -> x { _env = y }

--instance HasConcepts SynState where
--  concepts = lens _concepts \x y -> x { _concepts = y }

--instance HasFreshState SynState where
--  freshState = lens _fresh \x y -> x { _fresh = y }

--mkSynState :: Env -> MultiSet Concept -> SynState
--mkSynState e c = SynState mempty e c mkFreshState

--runSyn :: Monad m => RWST Mod () SynState m a ->
--  Mod -> SynState -> m (a, SynState)
--runSyn tc m g = do
--  (x, s, _) <- runRWST tc m g
--  return (x, s)

--evalSyn :: Monad m => RWST Mod () SynState m a ->
--  Mod -> SynState -> m a
--evalSyn tc m g = fst <$> runSyn tc m g

---- }}}

---- TODO: does init make sense? Maybe we should just have a module as input
---- and compute the GenState
--init :: SynMonad s m => Sketch -> m (Term Hole)
--init (Sketch _ t e) = do
--  (expr, _, ctx) <- check' e t
--  assign holeCtxs ctx
--  etaExpand $ strip expr

--step :: SynMonad s m => Term Hole -> m (Term Hole)
--step expr = do
--  i <- selectFirst
--  hf <- pick i
--  return $ fill (Map.singleton i hf) expr

--type SynMonad s m =
--  ( TCMonad m, MonadPlus m, FreshVar m
--  , MonadState s m, HasEnv s, HasConcepts s, HasCtxs s
--  )

---- Refinements {{{

---- This synthesis technique remembers every possible refinement for each hole,
---- and then prunes those that are conflicting when making a choice.

---- TODO: implement weighted search over the search space using a priority
---- queue. Perhaps weights should be adjustable, e.g. the weight of a function
---- could be lowered after usage, depending on how often we still expect the
---- function to appear.

--data Ref = Ref (Term Type) (Map Free Type)
--  deriving (Eq, Ord, Show)

--type Refs = Map Hole [Ref]

--restrictRef :: Map Free Type -> Ref -> Maybe Ref
--restrictRef th1 (Ref x th2) = do
--  th3 <- combine th1 th2
--  return $ Ref (over holes (subst th3) x) th3

--globals' :: (MonadState s m, HasEnv s, FreshFree m) => m [(Var, Poly)]
--globals' = do
--  xs <- Map.assocs <$> use environment
--  for xs \(x, (p, _)) -> do
--    q <- refresh p
--    return (x, q)

--refs :: (FreshFree m, MonadState s m, HasEnv s) => HoleCtx -> m [Ref]
--refs (HoleCtx t vs) = do
--  gs <- globals'
--  let ls = Map.assocs vs <&> second (Poly [])
--  return do
--    (x, Poly as (Args args res)) <- gs <> ls
--    case unify res t of
--      Nothing -> []
--      Just th -> do
--        let e = apps (Var x) (Hole . subst th <$> args)
--        let th' = Map.withoutKeys th $ Set.fromList as
--        [Ref e th']

--pick' :: SynMonad s m => Hole -> Ref -> Refs -> m (Term Hole, Refs)
--pick' h (Ref e th) rss = do
--  applySubst th -- TODO: is this needed?
--  -- Select and remove the holeCtx
--  ctx <- getCtx h
--  modifying holeCtxs $ Map.delete h
--  -- Process and eta expand refinement
--  x <- etaExpand =<< forOf holes e \u -> introduceHole (set goal u ctx)
--  let hs = Set.fromList $ toListOf holes x
--  -- Select holeCtxs of the new holes
--  ctxs' <- flip Map.restrictKeys hs <$> use holeCtxs
--  -- Compute all new refinements and restrict previous refinements
--  rss' <- traverse refs ctxs'
--  let rss'' = mapMaybe (restrictRef th) <$> Map.delete h rss
--  return (x, rss' <> rss'')

--next' :: SynMonad s m => Term Hole -> Refs -> m (Hole, Ref, Term Hole, Refs)
--next' e rss = do
--  (h, rs) <- mfold . Map.assocs $ rss
--  r <- mfold rs
--  (x, rss') <- pick' h r rss
--  return (h, r, fill (Map.singleton h x) e, rss')

--init' :: SynMonad s m => Sketch -> m (Hole, Ref, Term Hole, Refs)
--init' (Sketch _ t e) = do
--  (expr, _, ctx) <- check' e t
--  assign holeCtxs ctx
--  x <- etaExpand $ strip expr
--  use holeCtxs >>= traverse refs >>= next' x

--step' :: SynMonad s m => (Hole, Ref, Term Hole, Refs)
--  -> m (Hole, Ref, Term Hole, Refs)
--step' (_, _, e, rss) = next' e rss

---- }}}

--type HoleFilling = (Term Type, Type)

---- | Select the first hole to fill.
--selectFirst :: (MonadPlus m, MonadState s m, HasCtxs s) => m Hole
--selectFirst = use holeCtxs >>= fmap fst . mfold . Set.minView . Map.keysSet

---- | Try to select a valid hole filling for a hole.
--pick :: SynMonad s m => Hole -> m (Term Hole)
--pick h = do
--  -- Choose hole fillings from either local or global variables.
--  (hf, cs) <- locals h <|> globals <|> constructs
--  -- Check if the hole fillings fit.
--  e <- fillHole h hf
--  -- Remove the used concepts.
--  modifying concepts (`sub` fromSet cs)
--  -- Remove functions from the environment that use removed concepts
--  cs' <- use concepts
--  modifying environment . restrict $ Map.keysSet cs'
--  return e

---- | Performs type substitutions in holes and local variables.
--applySubst :: (MonadState s m, HasCtxs s) => Map Free Type -> m ()
--applySubst th = modifying holeCtxs . fmap $ over goal (subst th)

---- | Try and fill a hole using a hole filling.
--fillHole :: SynMonad s m => Hole -> HoleFilling -> m (Term Hole)
--fillHole h (e, t) = do
--  ctx <- getCtx h
--  -- Check if the hole filling fits.
--  th <- unify t (view goal ctx)
--  -- Introduce holes in the sketch.
--  x <- forOf holes e \u -> introduceHole (set goal u ctx)
--  -- Apply type substitutions to all relevant types.
--  applySubst th
--  -- Close the current hole.
--  modifying holeCtxs $ Map.delete h
--  -- Do postprocessing of the hole filling.
--  etaExpand x

---- | Introduce a new hole.
--introduceHole :: (FreshHole m, MonadState s m, HasCtxs s) => HoleCtx -> m Hole
--introduceHole ctx = do
--  h <- fresh
--  modifying holeCtxs $ Map.insert h ctx
--  return h

---- | Compute hole fillings from global variables.
--globals :: (MonadPlus m, FreshFree m, MonadState s m, HasEnv s) =>
--  m (HoleFilling, Set Concept)
--globals = do
--  (x, (t, c)) <- mfold . Map.assocs =<< use environment
--  u <- instantiateFresh t
--  return (fullyApply (Var x, u), c)

---- | Compute hole fillings from local variables.
--locals :: (MonadFail m, MonadPlus m, MonadState s m, HasCtxs s) =>
--  Hole -> m (HoleFilling, Set Concept)
--locals h = do
--  HoleCtx _ vs <- getCtx h
--  (v, t) <- mfold $ Map.assocs vs
--  return (fullyApply (Var v, t), Set.empty)

---- | Compute hole fillings from available language constructs.
---- TODO: I guess this is mzero for PointFree, and just pattern matching for
---- eta-long. Explicit lambda's could be used by a less restrictive synthesis
---- technique. Perhaps we should not try to generate language constructs
---- directly, but rather use eliminators, folds and fixpoints. However, it is
---- still useful to describe the associated holes fillings to trace student
---- solutions.
--constructs :: MonadPlus m => m (HoleFilling, Set Concept)
--constructs = mzero -- TODO

---- TODO: this does not take into account the fact that expressions might have a
---- polymorphic return type, i.e. might take any number of arguments given the
---- right instantiations.
--fullyApply :: HoleFilling -> HoleFilling
--fullyApply (e, Args ts u) = (apps e (Hole <$> ts), u)


---- TODO: this file is getting pretty messy, do some cleanup

---- NOTE: this variant of pick ignores concepts so that the same expression can
---- be used multiple times.
---- | Try to select a valid hole filling for a hole.
--pick'' :: SynMonad s m => Hole -> m (Term Hole)
--pick'' h = do
--  -- Choose hole fillings from either local or global variables.
--  (hf, _) <- locals h <|> globals <|> constructs
--  -- Check if the hole fillings fit.
--  fillHole h hf

---- TODO: replace fuel with a priority search queue.
---- NOTE: we expect that the expression is already eta-expanded when we start
---- guessing.
--guessUpTo :: SynMonad s m => Int -> HoleCtx -> m (Term Hole)
--guessUpTo i ctx = do
--  h <- fresh
--  assign holeCtxs $ Map.singleton h ctx
--  msum $ flip guessExact h <$> [1..i]

---- TODO: use some form of dynamic programming to reuse previously computed
---- results.
---- NOTE: each hole filling contributes 1 to the total size and the remaining
---- size is distributed over newly introduced holes.
--guessExact :: SynMonad s m => Int -> Hole -> m (Term Hole)
--guessExact 0 _ = mzero
--guessExact i h = do
--  hf <- pick'' h
--  -- TODO: this would be easier if we return a list of hole fillings instead of
--  -- an expression.
--  let hs = toListOf holes hf
--  case length hs of
--    -- NOTE: we only accept expresions of the exact size, to guarantee that we
--    -- do not generate the same expression more than once.
--    0 | i == 1 -> return hf
--    n -> do
--      is <- distr (i - 1) n
--      xs <- forM (zip is hs) \(i', h') -> (h',) <$> guessExact i' h'
--      return $ fill (Map.fromList xs) hf

---- TODO: implement this example using a file:
---- pretty$trySyn' @[] prelude'$solve "(\\xs -> { }) [1,2,3]" "List Nat" "[2,3,4]"
----
---- e.g.
----
---- import Prelude (map, succ)
----
---- addOne :: List Nat -> List Nat
---- addOne xs = {0}
----
---- assert 0 \[] -> []
---- assert 0 \[0,0] -> [1,1]
---- assert 0 \[1,2,3] -> [2,3,4]
-- }}}
