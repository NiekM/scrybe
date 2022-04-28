module Synthesis where

import Import
import Language
import qualified RIO.Map as Map
import qualified RIO.Set as Set

-- Synthesis state {{{

-- TODO: factor out EvalState

data SynState = SynState
  { _holeCtxs  :: Map Hole HoleCtx
  , _env       :: Env
  , _technique :: Technique
  , _concepts  :: MultiSet Concept
  , _fresh     :: FreshState
  }

instance HasCtxs SynState where
  holeCtxs = lens _holeCtxs \x y -> x { _holeCtxs = y }

instance HasEnv SynState where
  environment = lens _env \x y -> x { _env = y }

instance HasTech SynState where
  technique = lens _technique \x y -> x { _technique = y }

instance HasConcepts SynState where
  concepts = lens _concepts \x y -> x { _concepts = y }

instance HasFreshState SynState where
  freshState = lens _fresh \x y -> x { _fresh = y }

mkSynState :: Env -> Technique -> MultiSet Concept -> SynState
mkSynState e t c = SynState mempty e t c mkFreshState

runSyn :: Monad m => RWST (Module Void) () SynState m a ->
  Module Void -> SynState -> m (a, SynState)
runSyn tc m g = do
  (x, s, _) <- runRWST tc m g
  return (x, s)

evalSyn :: Monad m => RWST (Module Void) () SynState m a ->
  Module Void -> SynState -> m a
evalSyn tc m g = fst <$> runSyn tc m g

-- }}}

-- TODO: does init make sense? Maybe we should just have a module as input
-- and compute the GenState
init :: SynMonad s m => Sketch -> m (Term Hole)
init (Sketch _ t e) = do
  (expr, _, ctx) <- check' e t
  assign holeCtxs ctx
  postProcess (strip expr)

step :: SynMonad s m => Term Hole -> m (Term Hole)
step expr = do
  i <- selectFirst
  hf <- pick i
  return $ fill (Map.singleton i hf) expr

type SynMonad s m =
  ( TCMonad m, MonadPlus m, FreshVar m
  , MonadState s m, HasEnv s, HasConcepts s, HasTech s, HasCtxs s
  )

-- Refinements {{{

-- TODO: implement weighted search over the search space using a priority
-- queue. Perhaps weights should be adjustable, e.g. the weight of a function
-- could be lowered after usage, depending on how often we still expect the
-- function to appear.

-- TODO: should we use concepts here?
data Ref = Ref (Term Type) (Map Free Type) (Set Concept)
  deriving (Eq, Ord, Show)

type Refs = Map Hole [Ref]

restrictRef :: Map Free Type -> Ref -> Maybe Ref
restrictRef th1 (Ref x th2 c) = do
  th3 <- combine th1 th2
  return $ Ref (over holes (subst th3) x) th3 c

globals' :: (MonadState s m, HasEnv s, FreshFree m) =>
  m [(Var, Poly, Set Concept)]
globals' = do
  xs <- Map.assocs <$> use environment
  for xs \(x, (p, c)) -> do
    q <- refresh p
    return (x, q, c)

refs :: (FreshFree m, MonadState s m, HasEnv s) => HoleCtx -> m [Ref]
refs (HoleCtx t vs) = do
  gs <- globals'
  let ls = Map.assocs vs <&> \(a, u) -> (a, Poly [] u, Set.empty)
  return do
    (x, Poly as (Args args res), cs) <- gs <> ls
    case unify res t of
      Nothing -> []
      Just th -> do
        let e = apps (Var x) (Hole . subst th <$> args)
        let th' = Map.withoutKeys th $ Set.fromList as
        [Ref e th' cs]

pick' :: SynMonad s m => Hole -> Ref -> Refs -> m (Term Hole, Refs)
pick' h (Ref e th _cs) rss = do
  applySubst th -- TODO: is this needed?
  -- Select and remove the holeCtx
  ctxs <- use holeCtxs
  let ctx = fromMaybe (error "Missing hole") $ Map.lookup h ctxs
  modifying holeCtxs $ Map.delete h
  -- Process and etaExpand refinement
  x <- etaExpand =<< forOf holes e \u -> introduceHole (set goal u ctx)
  let hs = Set.fromList $ toListOf holes x
  -- Select holeCtxs of the new holes
  ctxs' <- flip Map.restrictKeys hs <$> use holeCtxs
  -- Compute all new refinements and restrict previous refinements
  rss' <- traverse refs ctxs'
  let rss'' = mapMaybe (restrictRef th) <$> Map.delete h rss
  return (x, rss' <> rss'')

next' :: SynMonad s m => Term Hole -> Refs -> m (Hole, Ref, Term Hole, Refs)
next' e rss = do
  (h, rs) <- mfold . Map.assocs $ rss
  r <- mfold rs
  (x, rss') <- pick' h r rss
  return (h, r, fill (Map.singleton h x) e, rss')

init' :: SynMonad s m => Sketch -> m (Hole, Ref, Term Hole, Refs)
init' (Sketch _ t e) = do
  (expr, _, ctx) <- check' e t
  assign holeCtxs ctx
  x <- postProcess (strip expr)
  use holeCtxs >>= traverse refs >>= next' x

step' :: SynMonad s m => (Hole, Ref, Term Hole, Refs)
  -> m (Hole, Ref, Term Hole, Refs)
step' (_, _, e, rss) = next' e rss

-- }}}

type HoleFilling = (Term Type, Type)

-- | Select the first hole to fill.
selectFirst :: (MonadPlus m, MonadState s m, HasCtxs s) => m Hole
selectFirst = use holeCtxs >>= fmap fst . mfold . Set.minView . Map.keysSet

-- | Try to select a valid hole filling for a hole.
pick :: SynMonad s m => Hole -> m (Term Hole)
pick h = do
  -- Choose hole fillings from either local or global variables.
  (hf, cs) <- locals h <|> globals <|> constructs
  -- Check if the hole fillings fit.
  e <- fillHole h hf
  -- Remove the used concepts.
  modifying concepts (`sub` fromSet cs)
  -- Remove functions from the environment that use removed concepts
  cs' <- use concepts
  modifying environment . restrict $ Map.keysSet cs'
  return e

-- | Performs type substitutions in holes and local variables.
applySubst :: (MonadState s m, HasCtxs s) => Map Free Type -> m ()
applySubst th = modifying holeCtxs . fmap $ over goal (subst th)

-- | Try and fill a hole using a hole filling.
fillHole :: SynMonad s m => Hole -> HoleFilling -> m (Term Hole)
fillHole h (e, t) = do
  ctx <- getCtx h
  -- Check if the hole filling fits.
  th <- unify t (view goal ctx)
  -- Introduce holes in the sketch.
  x <- forOf holes e \u -> introduceHole (set goal u ctx)
  -- Apply type substitutions to all relevant types.
  applySubst th
  -- Close the current hole.
  closeHole h
  -- Do postprocessing of the hole filling.
  postProcess x

-- | Process an expression.
postProcess :: (MonadFail m, MonadState s m, HasTech s, HasCtxs s, FreshVar m)
  => Term Hole -> m (Term Hole)
postProcess e = use technique >>= \case
  EtaLong -> etaExpand e
  _ -> return e

-- | Introduce a new hole.
introduceHole :: (FreshHole m, MonadState s m, HasCtxs s) => HoleCtx -> m Hole
introduceHole ctx = do
  h <- fresh
  modifying holeCtxs $ Map.insert h ctx
  return h

-- | Handles everything regarding the closing of holes.
-- NOTE: previously this made sure that we have no unused variables, but this
-- seems to be too much work for the gain, since variable uses are easy to
-- check anyways, as they introduce no new holes.
closeHole :: SynMonad s m => Hole -> m ()
closeHole h = modifying holeCtxs $ Map.delete h

-- | Compute hole fillings from global variables.
globals :: (MonadPlus m, FreshFree m, MonadState s m, HasTech s, HasEnv s) =>
  m (HoleFilling, Set Concept)
globals = do
  (x, (t, c)) <- mfold . Map.assocs =<< use environment
  u <- instantiateFresh t
  (,c) <$> holeFillings (Var x) u

-- | Compute hole fillings from local variables.
locals :: (MonadFail m, MonadPlus m, MonadState s m, HasTech s, HasCtxs s) =>
  Hole -> m (HoleFilling, Set Concept)
locals h = do
  HoleCtx _ vs <- getCtx h
  (v, t) <- mfold $ Map.assocs vs
  (,Set.empty) <$> holeFillings (Var v) t

-- | Compute hole fillings from available language constructs.
-- TODO: I guess this is mzero for PointFree, and just pattern matching for
-- eta-long. Explicit lambda's could be used by a less restrictive synthesis
-- technique. Perhaps we should not try to generate language constructs
-- directly, but rather use eliminators, folds and fixpoints. However, it is
-- still useful to describe the associated holes fillings to trace student
-- solutions.
constructs :: MonadPlus m => m (HoleFilling, Set Concept)
constructs = mzero -- TODO

-- | Compute the possible hole fillings from a function.
holeFillings :: (MonadPlus m, MonadState s m, HasTech s) =>
  Term Type -> Type -> m HoleFilling
holeFillings e t = use technique >>= \case
  EtaLong   -> return $ fullyApply (e, t)
  PointFree -> expand (e, t)

fullyApply :: HoleFilling -> HoleFilling
fullyApply (e, Args ts u) = (apps e (Hole <$> ts), u)

-- TODO: expand does not return 'all' ways to add holes to an
-- expression, since its return type might unify with a function type.
expand :: MonadPlus m => HoleFilling -> m HoleFilling
expand (e, t) = return (e, t) <|> case t of
  Arr t1 t2 -> expand (App e (Hole t1), t2)
  _ -> mzero
