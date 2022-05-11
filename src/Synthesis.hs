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
  , _concepts  :: MultiSet Concept
  , _fresh     :: FreshState
  }

instance HasCtxs SynState where
  holeCtxs = lens _holeCtxs \x y -> x { _holeCtxs = y }

instance HasEnv SynState where
  environment = lens _env \x y -> x { _env = y }

instance HasConcepts SynState where
  concepts = lens _concepts \x y -> x { _concepts = y }

instance HasFreshState SynState where
  freshState = lens _fresh \x y -> x { _fresh = y }

mkSynState :: Env -> MultiSet Concept -> SynState
mkSynState e c = SynState mempty e c mkFreshState

runSyn :: Monad m => RWST Module () SynState m a ->
  Module -> SynState -> m (a, SynState)
runSyn tc m g = do
  (x, s, _) <- runRWST tc m g
  return (x, s)

evalSyn :: Monad m => RWST Module () SynState m a ->
  Module -> SynState -> m a
evalSyn tc m g = fst <$> runSyn tc m g

-- }}}

-- TODO: does init make sense? Maybe we should just have a module as input
-- and compute the GenState
init :: SynMonad s m => Sketch -> m (Term Hole)
init (Sketch _ t e) = do
  (expr, _, ctx) <- check' e t
  assign holeCtxs ctx
  etaExpand $ strip expr

step :: SynMonad s m => Term Hole -> m (Term Hole)
step expr = do
  i <- selectFirst
  hf <- pick i
  return $ fill (Map.singleton i hf) expr

type SynMonad s m =
  ( TCMonad m, MonadPlus m, FreshVar m
  , MonadState s m, HasEnv s, HasConcepts s, HasCtxs s
  )

-- Refinements {{{

-- This synthesis technique remembers every possible refinement for each hole,
-- and then prunes those that are conflicting when making a choice.

-- TODO: implement weighted search over the search space using a priority
-- queue. Perhaps weights should be adjustable, e.g. the weight of a function
-- could be lowered after usage, depending on how often we still expect the
-- function to appear.

data Ref = Ref (Term Type) (Map Free Type)
  deriving (Eq, Ord, Show)

type Refs = Map Hole [Ref]

restrictRef :: Map Free Type -> Ref -> Maybe Ref
restrictRef th1 (Ref x th2) = do
  th3 <- combine th1 th2
  return $ Ref (over holes (subst th3) x) th3

globals' :: (MonadState s m, HasEnv s, FreshFree m) => m [(Var, Poly)]
globals' = do
  xs <- Map.assocs <$> use environment
  for xs \(x, (p, _)) -> do
    q <- refresh p
    return (x, q)

refs :: (FreshFree m, MonadState s m, HasEnv s) => HoleCtx -> m [Ref]
refs (HoleCtx t vs) = do
  gs <- globals'
  let ls = Map.assocs vs <&> second (Poly [])
  return do
    (x, Poly as (Args args res)) <- gs <> ls
    case unify res t of
      Nothing -> []
      Just th -> do
        let e = apps (Var x) (Hole . subst th <$> args)
        let th' = Map.withoutKeys th $ Set.fromList as
        [Ref e th']

pick' :: SynMonad s m => Hole -> Ref -> Refs -> m (Term Hole, Refs)
pick' h (Ref e th) rss = do
  applySubst th -- TODO: is this needed?
  -- Select and remove the holeCtx
  ctx <- getCtx h
  modifying holeCtxs $ Map.delete h
  -- Process and eta expand refinement
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
  x <- etaExpand $ strip expr
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
  modifying holeCtxs $ Map.delete h
  -- Do postprocessing of the hole filling.
  etaExpand x

-- | Introduce a new hole.
introduceHole :: (FreshHole m, MonadState s m, HasCtxs s) => HoleCtx -> m Hole
introduceHole ctx = do
  h <- fresh
  modifying holeCtxs $ Map.insert h ctx
  return h

-- | Compute hole fillings from global variables.
globals :: (MonadPlus m, FreshFree m, MonadState s m, HasEnv s) =>
  m (HoleFilling, Set Concept)
globals = do
  (x, (t, c)) <- mfold . Map.assocs =<< use environment
  u <- instantiateFresh t
  return (fullyApply (Var x, u), c)

-- | Compute hole fillings from local variables.
locals :: (MonadFail m, MonadPlus m, MonadState s m, HasCtxs s) =>
  Hole -> m (HoleFilling, Set Concept)
locals h = do
  HoleCtx _ vs <- getCtx h
  (v, t) <- mfold $ Map.assocs vs
  return (fullyApply (Var v, t), Set.empty)

-- | Compute hole fillings from available language constructs.
-- TODO: I guess this is mzero for PointFree, and just pattern matching for
-- eta-long. Explicit lambda's could be used by a less restrictive synthesis
-- technique. Perhaps we should not try to generate language constructs
-- directly, but rather use eliminators, folds and fixpoints. However, it is
-- still useful to describe the associated holes fillings to trace student
-- solutions.
constructs :: MonadPlus m => m (HoleFilling, Set Concept)
constructs = mzero -- TODO

-- TODO: this does not take into account the fact that expressions might have a
-- polymorphic return type, i.e. might take any number of arguments given the
-- right instantiations.
fullyApply :: HoleFilling -> HoleFilling
fullyApply (e, Args ts u) = (apps e (Hole <$> ts), u)


-- TODO: this file is getting pretty messy, do some cleanup

-- NOTE: this variant of pick ignores concepts so that the same expression can
-- be used multiple times.
-- | Try to select a valid hole filling for a hole.
pick'' :: SynMonad s m => Hole -> m (Term Hole)
pick'' h = do
  -- Choose hole fillings from either local or global variables.
  (hf, _) <- locals h <|> globals <|> constructs
  -- Check if the hole fillings fit.
  fillHole h hf

-- TODO: replace fuel with a priority search queue.
-- NOTE: we expect that the expression is already eta-expanded when we start
-- guessing.
guessUpTo :: SynMonad s m => Int -> HoleCtx -> m (Term Hole)
guessUpTo i ctx = do
  h <- fresh
  assign holeCtxs $ Map.singleton h ctx
  msum $ flip guessExact h <$> [1..i]

-- TODO: use some form of dynamic programming to reuse previously computed
-- results.
-- NOTE: each hole filling contributes 1 to the total size and the remaining
-- size is distributed over newly introduced holes.
guessExact :: SynMonad s m => Int -> Hole -> m (Term Hole)
guessExact 0 _ = mzero
guessExact i h = do
  hf <- pick'' h
  -- TODO: this would be easier if we return a list of hole fillings instead of
  -- an expression.
  let hs = toListOf holes hf
  case length hs of
    -- NOTE: we only accept expresions of the exact size, to guarantee that we
    -- do not generate the same expression more than once.
    0 | i == 1 -> return hf
    n -> do
      is <- distr (i - 1) n
      xs <- forM (zip is hs) \(i', h') -> (h',) <$> guessExact i' h'
      return $ fill (Map.fromList xs) hf

guessCheck :: SynMonad s m => Int -> Hole -> Goal -> m UC
guessCheck i h (vs, t, c) = do
  -- TODO: perhaps we need to check for a timeout?
  -- TODO: should we only guess at base types?
  e <- guessUpTo i $ HoleCtx t vs
  uh <- checkLive e c
  -- TODO: merge constraints?
  return (uh, Map.singleton h e)

ref :: SynMonad s m => UH -> Hole -> Goal -> m UC
ref uh h g = do
  (e, gs) <- refine g
  let uh' = uh <> fmap (view _3) gs
  modifying holeCtxs . Map.union $ fmap (\(u, ws, _) -> HoleCtx ws u) gs
  return (uh', Map.singleton h e)

-- Alternative version of ref that refines an expressionby checking if
-- unevaluation succeeds rather than explicitly inspecting the constraints to
-- see if the same constructor is used for each constraint.
refUneval :: SynMonad s m => UH -> Hole -> Goal -> m UC
refUneval uh h g = do
  (e, gs) <- refineUneval g
  let uh' = uh <> fmap (view _3) gs
  modifying holeCtxs . Map.union $ fmap (\(u, ws, _) -> HoleCtx ws u) gs
  return (uh', Map.singleton h e)

refineUneval :: SynMonad s m => Goal -> m (Term Hole, Map Hole Goal)
refineUneval (goalEnv, goalType, constraints) = do
  let constraints' = filter ((/= Top) . snd) constraints
  ctrs' <- ctrs <$> ask
  types' <- types <$> ask
  case goalType of
    Arr arg res -> do -- TODO: replace this just eta-expansion
      h <- fresh
      f <- fresh
      a <- fresh
      -- TODO: does it make sense to check all the constraints, or should we
      -- just eta-expand immediately, since that is the only reasonable result?
      xs <- failMaybe $ for constraints' \case
        (Scope scope, Lam t u) ->
          let r = App Fix (Scoped scope (Lam f (Lam a (Hole h))))
              scope' = Map.fromList [(f, r), (a, upcast t)]
          in Just (Scope $ scope' <> scope, u)
        _ -> Nothing
      -- TODO: record that f is a recursive call and a is its argument.
      -- Probably using a global variable environment.
      return
        ( App Fix (lams [f, a] (Hole h))
        , Map.singleton h
          ( Map.fromList [(f, goalType), (a, arg)] <> goalEnv
          , res
          , xs
          )
        )
    Apps (Ctr ctr) _ -> do
      -- TODO: check if this all makes sense with multiple examples
      cs <- mfold $ Map.lookup ctr (fmap fst . snd <$> types')
      c <- mfold cs
      Poly _ (Args args res) <- mfold $ Map.lookup c ctrs'
      th <- unify res goalType
      hs <- for args \u -> (,subst th u) <$> fresh
      let expr = apps (Ctr c) (Hole . fst <$> hs)
      uhs <- for constraints \(Scope scope, ex) -> uneval (eval scope expr) ex
      let uh = mergeUnsolved uhs
      xs <- for hs \(h, u) -> do
        constr <- mfold $ Map.lookup h uh
        return (h, (goalEnv, u, constr))
      return (expr, Map.fromList xs)
    _ -> fail "Failed refine"

solve :: SynMonad s m => Term Unit -> Poly -> Example -> m HF
solve e t x = do
  rs <- liveEnv <$> ask
  (strip -> e', _, ctxs) <- check' e t
  assign holeCtxs ctxs
  let r = eval rs e'
  uc <- uneval r x
  iterSolve (uc, mempty)

maxDepth :: Int
maxDepth = 4 -- TODO: move this into the synthesis monad.

iterSolve :: SynMonad s m => UC -> m HF
iterSolve (uh, hf) = case Map.minViewWithKey uh of
  Nothing -> return hf
  Just ((h, c), uh') -> do
    HoleCtx t vs <- getCtx h
    -- TODO: add refine & branch
    -- TODO: rewrite refine to use holeCtxs (and add Constraint to HoleCtx)
    -- uc <- refUneval uh' h (vs, t, c) <|> guessCheck 1 h (vs, t, c)
    uc <- ref uh' h (vs, t, c) <|> guessCheck maxDepth h (vs, t, c)
    -- TODO: simplify merged constraints
    uc' <- mfold $ merge [(uh', hf), uc]
    iterSolve uc'

-- TODO: implement this example using a file:
-- pretty$trySyn' @[] prelude'$solve "(\\xs -> { }) [1,2,3]" "List Nat" "[2,3,4]"
--
-- e.g.
--
-- import Prelude (map, succ)
--
-- addOne :: List Nat -> List Nat
-- addOne xs = {0}
--
-- assert 0 \[] -> []
-- assert 0 \[0,0] -> [1,1]
-- assert 0 \[1,2,3] -> [2,3,4]

-- TODO: use QuickSpec or similar techniques to compute properties of functions
-- as well as example sets, so that we can make an educated guess which
-- function would refine a certain hole.
-- It seems difficult in that e.g. the length-preserving property must be
-- computed on a non eta-expanded hole, or otherwise we would have to check the
-- properties in relation to local variables.
-- To implement this, we could make `refine` more abstract in that it would
-- check the consistency of example sets in relation to arbitrary properties,
-- rather than just "constructor consistency".
-- To figure out relevant properties to check, we could check the model
-- solutions as well as the traces generated by them and the available
-- functions in the environment, to have a sort of catalogus of properties
-- sorted by their relevance to the current exercise.
