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

selectFirst :: (MonadPlus m, WithHoleCtxs s m) => m (Hole, HoleCtx)
selectFirst = do
  ((i, ctx), _) <- use holeCtxs >>= mfold . Map.minViewWithKey
  modifying holeCtxs $ Map.delete i
  return (i, ctx)

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
    processSketch expr
  , step = \expr -> do
    (i, ctx) <- selectFirst
    hf <- pick ctx
    return $ subst (Map.singleton i hf) expr
  }

type SynMonad s m =
  ( WithEnvironment s m, WithConcepts s m
  , WithTechnique s m, WithHoleCtxs s m
  , FreshVar m, FreshHole m, FreshFree m
  , MonadFail m
  )

processSketch :: (MonadState s m, HasTechnique s, HasHoleCtxs s, FreshVar m)
  => Term Hole -> m (Term Hole)
processSketch e = use technique >>= \case
  EtaLong -> etaExpand e
  _ -> return e

type HoleFilling = (Term (Type Free), Type Free)

-- NOTE: this function does not take into account any newly introduced
-- variables in the sketch, this should be done separately with a call to
-- etaExpand
tryHoleFilling :: SynMonad s m => HoleCtx -> HoleFilling -> m (Term Hole)
tryHoleFilling HoleCtx { goal, local } (e, t) = do
  th <- unify t goal
  x <- forM e \u -> do
    h <- fresh
    modifying holeCtxs $ Map.insert h HoleCtx { goal = u, local }
    return h
  modifying holeCtxs . fmap $ substCtx th
  return x

pick :: (SynMonad s m, MonadPlus m) => HoleCtx -> m (Term Hole)
pick ctx = do
  -- Compute hole fillings from local variables.
  let locals = do
      (x, t) <- mfold . Map.assocs . local $ ctx
      fmap (,Set.empty) . holeFillings $ (Var x, t)
  -- Compute hole fillings from global variables.
  let globals = do
      (x, t, c) <- mfold =<< use environment
      fmap (,c) . holeFillings $ (Var x, t)
  -- Compute hole fillings from language constructs (lambdas, patterns, etc.)
  let constructs = mzero -- TODO
  -- Choose hole fillings from either local or global variables.
  (hf, cs) <- locals <|> globals <|> constructs
  -- Check if the hole fillings fit.
  e <- tryHoleFilling ctx hf
  -- Remove the used concepts.
  modifying concepts (`sub` fromSet cs)
  -- Remove functions from the environment that use removed concepts
  cs' <- use concepts
  modifying environment . restrict $ Map.keysSet cs'
  processSketch e

holeFillings :: (MonadPlus m, WithTechnique s m) => HoleFilling -> m HoleFilling
holeFillings hf = use technique >>= \case
  EtaLong   -> return . fullyApply $ hf
  PointFree -> expand hf

fullyApply :: HoleFilling -> HoleFilling
fullyApply (e, t) = first (apps . (e :|) . fmap Hole) (splitArgs t)

-- TODO: expand does not return 'all' ways to add holes to an
-- expression, since its return type might unify with a function type.
expand :: MonadPlus m => HoleFilling -> m HoleFilling
expand (e, t) = return (e, t) <|> case t of
  Arr t1 t2 -> expand (App e (Hole t1), t2)
  _ -> mzero
