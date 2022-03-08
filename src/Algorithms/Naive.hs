{-# LANGUAGE MultiWayIf #-}
module Algorithms.Naive where

import Import
import Language
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import TermGen

{-

Naive synthesis

This is a naive type-driven synthesizer that tries to fill each hole by
unifying its type with all possible instantiations of expressions in the
environment. It introduces no language constructs (lambda abstractions, case
analyses etc.). It does, however, use every expression in the environment only
once, to emulate simple synthesis parameters.

It is immediately clear how this synthesizer will try to fill in `compose` and
`foldr` at every step of the synthesis, since their return types are
polymorphic.

Rather than using `compose` to synthesize point-free expressions, we should
probably eta-expand each hole, in a sense inlining any compositions. Perhaps
all combinators, such as `id`, `const` and `flip`, should be disallowed, as
they all try to serve the same purpose that lambda abstractions already
provide.

Even though we do not want to use `compose` and friends, `foldr` is a very
useful function that we do want to use during synthesis. It should, however,
come with some restrictions, similar to the ones we would put on case analyses
and recursive calls, since `foldr` uses both internally.

-}

data Syn = Syn
  -- TODO: does init make sense? Maybe we should just have a module as input
  -- and compute the GenState
  { init :: Dec -> GenT Maybe (Term Hole)
  , step :: Term Hole -> GenT [] (Term Hole)
  }

synth :: Syn
synth = Syn
  { init = \dec -> do
    (expr, _, _, ctx) <- check dec
    assign holeCtxs ctx
    postProcess expr
  , step = \expr -> do
    i <- selectFirst
    hf <- pick i
    return $ subst (Map.singleton i hf) expr
  }

type SynMonad s m =
  ( WithEnvironment s m, WithConcepts s m
  , WithTechnique s m, WithHoleCtxs s m
  , WithVariables s m
  , FreshVarId m, FreshHole m, FreshFree m
  , MonadFail m
  , MonadPlus m
  )

type HoleFilling = (Term (Type Free), Type Free)

-- | Select the first hole to fill.
selectFirst :: (MonadPlus m, WithHoleCtxs s m) => m Hole
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

-- | Try and fill a hole using a hole filling.
fillHole :: SynMonad s m => Hole -> HoleFilling -> m (Term Hole)
fillHole h (e, t) = do
  HoleCtx { goal, local } <- getCtx h
  -- Check if the hole filling fits.
  th <- unify t goal
  -- Introduce holes in the sketch.
  x <- forM e \u -> introduceHole HoleCtx { goal = u, local }
  -- Apply type substitutions to all relevant types.
  applySubst th
  -- Close the current hole.
  closeHole h
  -- Do postprocessing of the hole filling.
  postProcess x

-- | Process an expression.
postProcess :: (WithTechnique s m, WithHoleCtxs s m, WithVariables s m, FreshVarId m)
  => Term Hole -> m (Term Hole)
postProcess e = use technique >>= \case
  EtaLong -> etaExpand e
  _ -> return e

-- | Retrieve the context of a hole.
getCtx :: (MonadFail m, WithHoleCtxs s m) => Hole -> m HoleCtx
getCtx h = use holeCtxs >>=
  maybe (fail "Missing holeCtx") return . Map.lookup h

-- | Introduce a new hole.
introduceHole :: (FreshHole m, WithHoleCtxs s m, WithVariables s m) =>
  HoleCtx -> m Hole
introduceHole ctx = do
  h <- fresh
  modifying holeCtxs $ Map.insert h ctx
  forM_ (local ctx) $ modifying variables . Map.adjust
    \(Variable x t n m) -> Variable x t (n + 1) m
  return h

-- | Handles everything regarding the closing of holes.
closeHole :: SynMonad s m => Hole -> m ()
closeHole h = do
  HoleCtx { local } <- getCtx h
  modifying holeCtxs $ Map.delete h
  xs <- use variables
  forM_ local \i -> case Map.lookup i xs of
    Nothing -> fail $ "Missing variable id " <> show i
    Just (Variable x t n m) ->
      if | n <= 1, m == 0 -> fail $ "Unused variable " <> show x
         -- TODO: why does this break? make sure the number of occurences is
         -- kept track of correctly.
         --  | n <= 1 -> modifying variables $ Map.delete i
         | otherwise ->
           modifying variables $ Map.insert i (Variable x t (n - 1) m)

-- | Performs type substitutions in holes and local variables.
applySubst :: (WithHoleCtxs s m, WithVariables s m) =>
  Map Free (Type Free) -> m ()
applySubst th = do
  modifying holeCtxs $ fmap \ctx -> ctx { goal = subst th $ goal ctx }
  modifying variables $ fmap \(Variable x t i n) -> Variable x (subst th t) i n

-- TODO: rather than strictly disallowing some holefillings, we should use
-- weights to discourage them.

-- | Compute hole fillings from global variables.
globals :: (MonadPlus m, FreshFree m, WithTechnique s m, WithEnvironment s m) =>
  m (HoleFilling, Set Concept)
globals = do
  (x, t, c) <- mfold =<< use environment
  u <- renumber t
  (,c) <$> holeFillings x u

-- | Compute hole fillings from local variables.
locals :: (MonadFail m, MonadPlus m,
  WithVariables s m, WithTechnique s m, WithHoleCtxs s m) =>
  Hole -> m (HoleFilling, Set Concept)
locals h = do
  HoleCtx { local } <- getCtx h
  i <- mfold local
  xs <- use variables
  Variable x t n m <- mfold $ Map.lookup i xs
  -- Note variable occurrence
  modifying variables . Map.insert i $ Variable x t n (m + 1)
  (,Set.empty) <$> holeFillings x t

-- | Compute hole fillings from available language constructs.
-- TODO: I guess this is mzero for PointFree, and just pattern matching for
-- eta-long. Explicit lambda's could be used by a less restrictive synthesis
-- technique.
constructs :: MonadPlus m => m (HoleFilling, Set Concept)
constructs = mzero -- TODO

-- | Compute the possible hole fillings from a function.
holeFillings :: (MonadPlus m, WithTechnique s m) =>
  Var -> Type Free -> m HoleFilling
holeFillings x t = use technique >>= \case
  EtaLong   -> return $ fullyApply (Var x, t)
  PointFree -> expand (Var x, t)

fullyApply :: HoleFilling -> HoleFilling
fullyApply (e, t) = first (apps . (e :|) . fmap Hole) (splitArgs t)

-- TODO: expand does not return 'all' ways to add holes to an
-- expression, since its return type might unify with a function type.
expand :: MonadPlus m => HoleFilling -> m HoleFilling
expand (e, t) = return (e, t) <|> case t of
  Arr t1 t2 -> expand (App e (Hole t1), t2)
  _ -> mzero
