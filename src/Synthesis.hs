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
  , _tcState   :: TCState
  }

instance HasCtxs SynState where
  holeCtxs = lens _holeCtxs \x y -> x { _holeCtxs = y }

instance HasEnv SynState where
  environment = lens _env \x y -> x { _env = y }

instance HasTech SynState where
  technique = lens _technique \x y -> x { _technique = y }

instance HasConcepts SynState where
  concepts = lens _concepts \x y -> x { _concepts = y }

instance HasTCState SynState where
  tcState = lens _tcState \x y -> x { _tcState = y }

instance HasVars SynState where
  variables = tcState . variables

instance HasFreshState SynState where
  freshState = tcState . freshState

mkSynState :: Env -> Technique -> MultiSet Concept -> SynState
mkSynState e t c = SynState mempty e t c mkTCState

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
init :: SynMonad s m => Sketch -> m (Term Var Hole)
init (Sketch _ t e) = do
  (expr, _, ctx) <- check e t
  assign holeCtxs ctx
  postProcess (strip expr)

step :: SynMonad s m => Term Var Hole -> m (Term Var Hole)
step expr = do
  i <- selectFirst
  hf <- pick i
  return $ fill (Map.singleton i hf) expr

type SynMonad s m =
  ( MonadReader (Module Void) m, MonadState s m
  , HasEnv s, HasConcepts s , HasTech s, HasCtxs s , HasVars s
  , FreshVarId m, FreshHole m, FreshFree m
  , MonadFail m , MonadPlus m
  )

-- Refinements {{{

-- TODO: implement weighted search over the search space using a priority
-- queue. Perhaps weights should be adjustable, e.g. the weight of a function
-- could be lowered after usage, depending on how often we still expect the
-- function to appear.

-- TODO: should we use concepts here?
data Ref = Ref (Term Var Type) (Map Var Type) (Set Concept)
  deriving (Eq, Ord, Show)

type Refs = Map Hole [Ref]

restrictRef :: Map Var Type -> Ref -> Maybe Ref
restrictRef th1 (Ref x th2 c) = do
  th3 <- combine th1 th2
  return $ Ref (over holes (subst th3) x) th3 c

globals' :: (MonadState s m, HasEnv s, FreshFree m) =>
  m [(Var, Poly, Set Concept)]
globals' = do
  xs <- Map.assocs <$> use environment
  for xs \(x, (Poly as p, c)) -> do
    th <- for as \a -> (a,) . freeId <$> fresh
    let u = subst (Var <$> Map.fromList th) p
    return (x, Poly (snd <$> th) u, c)

holeVars :: (MonadState s m, HasVars s) =>
  Map Hole HoleCtx -> m (Map Hole (Type, Map Var Type))
holeVars ctxs = do
  vs <- use variables
  for ctxs \(HoleCtx t ids) ->
    return . (t,) $ ids <&> \i -> case Map.lookup i vs of
      Nothing -> error "Missing VarId: this should never happen"
      Just (Variable _ u _ _) -> u

refs :: (FreshFree m, MonadState s m, HasEnv s) =>
  Type -> Map Var Type -> m [Ref]
refs t vs = do
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

pick' :: (FreshFree m, FreshHole m, FreshVarId m, MonadState s m)
  => (HasEnv s, HasVars s, HasCtxs s)
  => Hole -> Ref -> Refs -> m (Term Var Hole, Refs)
pick' h (Ref e th _cs) rss = do
  -- Select the holeCtx and remove it from holeCtxs
  applySubst th
  ctxs <- use holeCtxs
  let ctx = case Map.lookup h ctxs of
        Nothing -> error ("This should never happen: " <> show h)
        Just x -> x
  modifying holeCtxs $ Map.delete h
  -- Process and etaExpand refinement
  x <- etaExpand =<< forOf holes e \u -> introduceHole (set goal u ctx)
  let hs = Set.fromList $ toListOf holes x
  ctxs' <- flip Map.restrictKeys hs <$> use holeCtxs
  rss' <- traverse (uncurry refs) =<< holeVars ctxs'
  let rss'' = mapMaybe (restrictRef th) <$> Map.delete h rss
  return (x, rss' <> rss'')

next' :: SynMonad s m => Term Var Hole -> Refs ->
  m (Hole, Ref, Term Var Hole, Refs)
next' e rss = do
  (h, rs) <- mfold . Map.assocs $ rss
  r <- mfold rs
  (x, rss') <- pick' h r rss
  return (h, r, fill (Map.singleton h x) e, rss')

init' :: SynMonad s m => Sketch -> m (Hole, Ref, Term Var Hole, Refs)
init' (Sketch _ t e) = do
  (expr, _, ctx) <- check e t
  assign holeCtxs ctx
  x <- postProcess (strip expr)
  use holeCtxs >>= holeVars >>= traverse (uncurry refs) >>= next' x

step' :: SynMonad s m => (Hole, Ref, Term Var Hole, Refs)
  -> m (Hole, Ref, Term Var Hole, Refs)
step' (_, _, e, rss) = next' e rss

-- }}}

type HoleFilling = (Term Var Type, Type)

-- | Select the first hole to fill.
selectFirst :: (MonadPlus m, MonadState s m, HasCtxs s) => m Hole
selectFirst = use holeCtxs >>= fmap fst . mfold . Set.minView . Map.keysSet

-- | Try to select a valid hole filling for a hole.
pick :: SynMonad s m => Hole -> m (Term Var Hole)
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

-- | Try and fill a hole using a hole filling.
fillHole :: SynMonad s m => Hole -> HoleFilling -> m (Term Var Hole)
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
postProcess :: (MonadState s m, HasTech s, HasCtxs s, HasVars s, FreshVarId m)
  => Term Var Hole -> m (Term Var Hole)
postProcess e = use technique >>= \case
  EtaLong -> etaExpand e
  _ -> return e

-- | Retrieve the context of a hole.
getCtx :: (MonadFail m, MonadState s m, HasCtxs s) => Hole -> m HoleCtx
getCtx h = use holeCtxs >>=
  maybe (fail "Missing holeCtx") return . Map.lookup h

-- | Introduce a new hole.
introduceHole :: (FreshHole m, MonadState s m, HasCtxs s, HasVars s)
  => HoleCtx -> m Hole
introduceHole ctx = do
  h <- fresh
  modifying holeCtxs $ Map.insert h ctx
  forM_ (view local ctx) $ modifying variables . Map.adjust
    \(Variable x t n m) -> Variable x t (n + 1) m
  return h

-- | Handles everything regarding the closing of holes.
closeHole :: SynMonad s m => Hole -> m ()
closeHole h = do
  ctx <- getCtx h
  modifying holeCtxs $ Map.delete h
  xs <- use variables
  forM_ (view local ctx) \i -> case Map.lookup i xs of
    Nothing -> fail $ "Missing variable id " <> show i
    Just (Variable x t n m)
      | n <= 1, m == 0 -> fail $ "Unused variable " <> show x
      | otherwise ->
        modifying variables $ Map.insert i (Variable x t (n - 1) m)

-- | Performs type substitutions in holes and local variables.
applySubst :: (MonadState s m, HasCtxs s, HasVars s) =>
  Map Var Type -> m ()
applySubst th = do
  modifying holeCtxs . fmap $ over goal (subst th)
  modifying variables . fmap $ over varType (subst th)

-- TODO: rather than strictly disallowing some holefillings, we should use
-- weights to discourage them.

-- | Compute hole fillings from global variables.
globals :: (MonadPlus m, FreshFree m, MonadState s m, HasTech s, HasEnv s) =>
  m (HoleFilling, Set Concept)
globals = do
  (x, (t, c)) <- mfold . Map.assocs =<< use environment
  u <- instantiateFresh t
  (,c) <$> holeFillings (Var x) u

-- | Compute hole fillings from local variables.
locals :: (MonadFail m, MonadPlus m) =>
  (MonadState s m, HasVars s, HasTech s, HasCtxs s) =>
  Hole -> m (HoleFilling, Set Concept)
locals h = do
  ctx <- getCtx h
  i <- mfold (view local ctx)
  xs <- use variables
  Variable x t n m <- mfold $ Map.lookup i xs
  -- Note variable occurrence
  modifying variables . Map.insert i $ Variable x t n (m + 1)
  (,Set.empty) <$> holeFillings (Var x) t

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
  Term Var Type -> Type -> m HoleFilling
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
