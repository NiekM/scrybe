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

getCtrs :: Term Hole -> [Ctr]
getCtrs = \case
  Ctr c -> [c]
  App f x -> getCtrs f ++ getCtrs x
  Lam _ x -> getCtrs x
  _ -> []

getVars :: Term Hole -> [Var]
getVars = \case
  Var v -> [v]
  App f x -> getVars f ++ getVars x
  Lam _ x -> getVars x
  _ -> []

selectFirst :: (MonadPlus m, WithHoleCtxs s m) => m (Hole, HoleCtx)
selectFirst = do
  ((i, ctx), _) <- use holeCtxs >>= mfold . Map.minViewWithKey
  modifying holeCtxs $ Map.delete i
  return (i, ctx)

tryReduce :: (Maybe Int, a) -> Maybe (Maybe Int, a)
tryReduce (i, x) = case i of
  Nothing -> Just (Nothing, x)
  Just n | n > 1 -> Just (Just $ n - 1, x)
  _ -> Nothing

-- TODO: note that there are no restrictions on local variables, as it would be
-- pretty tricky take into account which holes share this expression.
holeFillings :: (Ord a, MonadPlus m, FreshFree m, WithEnv s m) =>
  Map Var (Type Free) -> m (Term a, Type Free)
holeFillings local = mfold (Map.assocs $ Map.mapKeys Var local) <|> do
  (x, (_, t)) <- mfold . Map.assocs =<< use env
  modifying env $ Map.update tryReduce x
  u <- renumber t
  return (absurd <$> x, u)

-- TODO: this should probably take declaration
addHoles :: (FreshHole m, FreshVar m, WithHoleCtxs s m) =>
  Map Var (Type Free) -> Term Hole -> Type Free -> m (Term Hole, Type Free)
addHoles local e t = do
  let (us, u) = splitArgs t
  hs <- for us \goal -> do
    h <- fresh
    modifying holeCtxs $ Map.insert h HoleCtx { goal, local }
    return $ Hole h
  (,u) <$> etaExpand (apps $ e :| hs)

expand :: (MonadPlus m, FreshHole m, FreshVar m, WithHoleCtxs s m) =>
  Map Var (Type Free) -> Term Hole -> Type Free -> m (Term Hole, Type Free)
expand local e t = return (e, t) <|> case t of
  Arr t1 t2 -> do
    h <- fresh
    modifying holeCtxs $ Map.insert h HoleCtx { goal = t1, local }
    expand local (App e (Hole h)) t2
  _ -> mzero

data Syn = Syn
  -- TODO: does init make sense? Maybe we should just have a module as input
  -- and compute the GenState
  { init :: Dec -> GenT Maybe (Term Hole)
  , step :: Term Hole -> GenT [] (Term Hole)
  }

-- Naive {{{

naive :: Syn
naive = Syn
  { init = \dec -> do
    (expr, _, _, ctx) <- check dec
    assign holeCtxs ctx
    -- TODO: only remove one occurrence from the environment for every
    -- occurrence in the expression.
    modifying env $ Map.filterWithKey \x _ ->
      x `notElem` (fmap Ctr (getCtrs expr) ++ fmap Var (getVars expr))
    return expr
  , step = \ expr -> do
    -- Select the first hole.
    (i, HoleCtx { goal, local }) <- selectFirst
    -- Pick an expression from the environment.
    -- TODO: remove used vars form environment while selecting them
    (e, t) <- holeFillings local
    -- Compute all ways to add holes to the expression.
    (hf, ty) <- expand local e t
    -- Try to unify with the goal type
    th <- unify ty goal
    -- Update the hole contexts.
    modifying holeCtxs $ fmap (substCtx th)
    -- Fill the selected hole.
    return $ subst (Map.singleton i hf) expr
  }

-- }}}

-- Eta-long {{{

eta :: Syn
eta = Syn
  { init = \dec -> do
    (expr, _, _, ctx) <- check dec
    assign holeCtxs ctx
    etaExpand expr

  , step = \expr -> do
    -- Select the first hole.
    (i, HoleCtx { goal, local }) <- selectFirst
    -- Pick an expression from the environment.
    (e, t) <- holeFillings local
    -- Add new holes to stay in eta-long form.
    (hf, ty) <- addHoles local e t
    -- Try to unify with the goal type.
    th <- unify ty goal
    -- Update the hole contexts.
    modifying holeCtxs $ fmap (substCtx th)
    -- Fill the selected hole.
    return $ subst (Map.singleton i hf) expr
  }

-- }}}
