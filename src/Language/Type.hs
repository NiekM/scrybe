{-# LANGUAGE GADTs #-}
module Language.Type where

import Import
import Fresh
import Language.Syntax
import Language.Utils
import qualified RIO.Map as Map

type Unify a = Map a (Type a)

-- TODO: Add unit tests to test type unification, inference and checking

-- | Unify two types, by checking if their holes can be filled such that
-- they are equivalent.
unify :: (MonadFail m, Ord a) => Type a -> Type a -> m (Unify a)
unify t u = case (t, u) of
  (App t1 t2, App u1 u2) -> unifies [(t1, u1), (t2, u2)]
  (Var  a, Var  b) | a == b -> return Map.empty
  (Ctr  a, Ctr  b) | a == b -> return Map.empty
  (Hole a, Hole b) | a == b -> return Map.empty
  (Hole a, _) | occurs a u -> return $ Map.singleton a u
  (_, Hole a) | occurs a t -> return $ Map.singleton a t
  _ -> fail "Unification failed"

occurs :: Ord a => a -> Type a -> Bool
occurs a tau = a `notElem` free tau

unifies :: (MonadFail m, Foldable t, Ord a) =>
  t (Type a, Type a) -> m (Unify a)
unifies = flip foldr (return Map.empty) \(t1, t2) th -> do
  th0 <- th
  th1 <- unify (subst th0 t1) (subst th0 t2)
  return $ compose th1 th0

freshHole :: MonadFresh s m => m (Expr l s)
freshHole = Hole <$> fresh

infer :: (MonadFresh Free m, MonadFail m) => Module -> Term Hole ->
  m (Type Free, Unify Free, Map Hole HoleCtx)
infer Module { ctrs, vars } = go Map.empty where
  go env = \case
    Hole i -> do
      t <- freshHole
      return (t, Map.empty, Map.singleton i (t, env))
    Ctr c -> do
      t <- failMaybe $ Map.lookup c ctrs
      u <- renumber t
      return (u, Map.empty, Map.empty)
    Var a | Just t <- Map.lookup a env -> return (t, Map.empty, Map.empty)
    Var a -> do
      t <- failMaybe $ Map.lookup a vars
      u <- renumber t
      return (u, Map.empty, Map.empty)
    App f x -> do
      (a, th1, ctx1) <- go env f
      (b, th2, ctx2) <- go (subst th1 <$> env) x
      t <- Hole <$> fresh
      th3 <- unify (subst th2 a) (Arr b t)
      let th4 = th3 `compose` th2 `compose` th1
      let ctx3 = (subst th4 *** fmap (subst th4)) <$> ctx1 <> ctx2
      return (subst th4 t, th4, ctx3)
    Lam x e -> do
      t <- freshHole
      (u, th, ctx) <- go (Map.insert x t env) e
      return (subst th t `Arr` u, th, ctx)
    Case xs -> undefined

-- TODO: maybe this should return a sketch along with a type and unification
check ::
  (MonadReader Module m, MonadFresh Free m, MonadFresh Hole m, MonadFail m) =>
  Dec -> m (Term Hole, Type Free, Unify Free, Map Hole HoleCtx)
check (Dec t e) = do
  e' <- fmap fst <$> number e
  env <- ask
  (u, th1, ctx1) <- infer env e'
  th2 <- unify t u
  let th3 = compose th2 th1
  let ctx2 = (subst th3 *** fmap (subst th3)) <$> ctx1
  return (e', subst th3 u, th3, ctx2)
