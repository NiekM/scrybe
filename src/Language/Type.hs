{-# LANGUAGE GADTs #-}
module Language.Type where

import Import
import Language.Syntax
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import Control.Monad.State

free :: Ord a => Type a -> Set a
free = Set.fromList . toList

-- | Unify two types, by checking if their holes can be filled such that
-- they are equivalent.
unify :: Ord a => Type a -> Type a -> Maybe (Map a (Type a))
unify t u = case (t, u) of
  (App t1 t2, App u1 u2) -> unifies [(t1, u1), (t2, u2)]
  (Var  a, Var  b) | a == b -> return Map.empty
  (Ctr  a, Ctr  b) | a == b -> return Map.empty
  (Hole a, Hole b) | a == b -> return Map.empty
  -- (Hole a, Hole b) -> return $ Map.fromList [(a, Hole b), (b, Hole a)]
  (Hole a, _) | occursCheck a u -> return $ Map.singleton a u
  (_, Hole a) | occursCheck a t -> return $ Map.singleton a t
  _ -> Nothing

occursCheck :: Ord a => a -> Type a -> Bool
occursCheck a tau = a `notElem` free tau

unifies :: Ord a => [(Type a, Type a)] -> Maybe (Map a (Type a))
unifies = flip foldr (return Map.empty) \(t1, t2) th -> do
  th0 <- th
  th1 <- unify (subst th0 t1) (subst th0 t2)
  return $ compose th0 th1

infer :: Module -> Term Hole -> StateT Free Maybe
  (Type Free, Map Free (Type Free), Map Hole HoleCtx)
infer Module { ctrs, vars } = go Map.empty where
  go env = \case
    Hole i -> do
      n <- get
      put (1 + n)
      let t = Hole n
      return (t, Map.empty, Map.singleton i (t, env))
    Ctr c -> do
      t <- lift $ Map.lookup c ctrs
      n <- get
      let u = (n +) <$> t
      put (1 + maximumDef (n - 1) u)
      return (u, Map.empty, Map.empty)
    Var a | Just t <- Map.lookup a env -> return (t, Map.empty, Map.empty)
    Var a -> do
      t <- lift $ Map.lookup a vars
      n <- get
      let u = (n +) <$> t
      put (1 + maximumDef (n - 1) u)
      return (u, Map.empty, Map.empty)
    App f x -> do
      (a, th1, ctx1) <- go env f
      (b, th2, ctx2) <- go (subst th1 <$> env) x
      n <- get
      let t = Hole n
      put (n + 1)
      th3 <- lift $ unify (subst th2 a) (Arr b t)
      let th4 = th3 `compose` th2 `compose` th1
      let ctx3 = (subst th4 *** fmap (subst th4)) <$> ctx1 <> ctx2
      return (subst th4 t, th4, ctx3)
    Lam x e -> do
      n <- get
      let t = Hole n
      put (n + 1)
      (u, th, ctx) <- go (Map.insert x t env) e
      return (subst th t `Arr` u, th, ctx)
    Case xs -> undefined

check :: Module -> Term Hole -> Type Free -> StateT Free Maybe
  (Type Free, Map Free (Type Free), Map Hole HoleCtx)
check m e t = do
  (u, th1, ctx1) <- infer m e
  th2 <- lift $ unify t u
  let th3 = compose th2 th1
  let ctx2 = (subst th3 *** fmap (subst th3)) <$> ctx1
  return (subst th3 u, th3, ctx2)
