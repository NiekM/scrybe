{-# LANGUAGE GADTs #-}
module Language.Type where

import Import
import Language.Syntax
import Language.Utils
import qualified RIO.Map as Map
import qualified RIO.Set as Set

type Unify l a = Map a (Expr l a)

-- TODO: Add unit tests to test type unification, inference and checking

-- TODO: unify should also work on patterns!
-- TODO: can types and patterns not have holes other than free variables?

-- | Unify two expressions, by checking if their holes can be filled such that
-- they are equivalent.
unify :: (MonadFail m, Ord a) => Expr l a -> Expr l a -> m (Unify l a)
unify t u = case (t, u) of
  (App t1 t2, App u1 u2) -> unifies [(t1, u1), (t2, u2)]
  (Var  a, Var  b) | a == b -> return Map.empty
  (Ctr  a, Ctr  b) | a == b -> return Map.empty
  (Hole a, Hole b) | a == b -> return Map.empty
  (Hole a, _) | occurs a u -> return $ Map.singleton a u
  (_, Hole a) | occurs a t -> return $ Map.singleton a t
  _ -> fail "Unification failed"

occurs :: Ord a => a -> Expr l a -> Bool
occurs a tau = a `notElem` Set.fromList (holes tau)

unifies :: (MonadFail m, Foldable t, Ord a) =>
  t (Expr l a, Expr l a) -> m (Unify l a)
unifies = flip foldr (return Map.empty) \(t1, t2) th -> do
  th0 <- th
  th1 <- unify (subst th0 t1) (subst th0 t2)
  return $ compose th1 th0

infer :: (MonadFresh Free m, MonadFail m, MonadReader Module m) =>
  Term Hole -> m (Type Free, Unify 'Type Free, Map Hole HoleCtx)
infer expr = do
  Module { ctrs, vars } <- ask
  let go local = \case
        Hole i -> do
          goal <- Hole <$> fresh
          return (goal, Map.empty, Map.singleton i HoleCtx { goal, local })
        Ctr c -> do
          t <- failMaybe $ Map.lookup c ctrs
          u <- renumber t
          return (u, Map.empty, Map.empty)
        Var a | Just t <- Map.lookup a local -> return (t, Map.empty, Map.empty)
        Var a -> do
          t <- failMaybe $ Map.lookup a vars
          u <- renumber t
          return (u, Map.empty, Map.empty)
        App f x -> do
          (a, th1, ctx1) <- go local f
          (b, th2, ctx2) <- go (subst th1 <$> local) x
          t <- Hole <$> fresh
          th3 <- unify (subst th2 a) (Arr b t)
          let th4 = th3 `compose` th2 `compose` th1
          let ctx3 = substCtx th4 <$> ctx1 <> ctx2
          return (subst th4 t, th4, ctx3)
        Lam x e -> do
          t <- Hole <$> fresh
          (u, th, local') <- go (Map.insert x t local) e
          return (subst th t `Arr` u, th, local')
        Case _xs -> undefined
  go Map.empty expr

-- TODO: maybe this should return a sketch along with a type and unification
check :: (MonadFresh Free m, MonadFresh Hole m, MonadFail m,
  MonadReader Module m) =>
  Dec -> m (Term Hole, Type Free, Unify 'Type Free, Map Hole HoleCtx)
check (Dec t e) = do
  e' <- fmap fst <$> number e
  (u, th1, ctx1) <- infer e'
  th2 <- unify t u
  let th3 = compose th2 th1
  let ctx2 = substCtx th3 <$> ctx1
  return (e', subst th3 u, th3, ctx2)
