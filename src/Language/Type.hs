{-# LANGUAGE GADTs, MultiParamTypeClasses #-}
module Language.Type where

import Import
import Language.Syntax
import qualified RIO.Map as Map

-- Type checking state {{{

runTC :: Monad m => RWST (Module Void) () FreshState m a ->
  Module Void -> m (a, FreshState)
runTC tc m = do
  (x, s, _) <- runRWST tc m mkFreshState
  return (x, s)

evalTC :: Monad m => RWST (Module Void) () FreshState m a -> Module Void -> m a
evalTC tc m = fst <$> runTC tc m

-- }}}

-- | Type unification & inference

type Unify = Map Free Type

-- TODO: Add unit tests to test type unification, inference and checking

-- | Unify two expressions, by checking if their holes can be filled such that
-- they are equivalent.
unify :: MonadFail m => Type -> Type -> m Unify
unify t u = case (t, u) of
  (App t1 t2, App u1 u2) -> unifies [(t1, u1), (t2, u2)]
  (Var  a, Var  b) | a == b -> return Map.empty
  (Ctr  a, Ctr  b) | a == b -> return Map.empty
  (Var a, _) | occurs a u -> return $ Map.singleton a u
  (_, Var a) | occurs a t -> return $ Map.singleton a t
  _ -> fail "Unification failed"

occurs :: Free -> Type -> Bool
occurs a tau = a `notElem` toListOf free tau

-- NOTE: it seems that the left hand side of the composition should be the
-- newer composition, in effect updating the old substitution according to the
-- new ones
-- | Compose two non-conflicting unifications.
compose :: Unify -> Unify -> Unify
compose sigma gamma = Map.unions
  [ subst sigma <$> gamma
  , Map.withoutKeys sigma (Map.keysSet gamma)
  ]

-- | Unify multiple expressions.
unifies :: (MonadFail m, Foldable t) => t (Type, Type) -> m Unify
unifies = flip foldr (return Map.empty) \(t1, t2) th -> do
  th0 <- th
  th1 <- unify (subst th0 t1) (subst th0 t2)
  return $ compose th1 th0

-- | Try to combine possibly conflicting unifications.
combine :: MonadFail m => Unify -> Unify -> m Unify
combine th1 th2 = foldr (\y z -> z >>= go y)
  (return $ subst th1 <$> th2) $ Map.assocs th1 where
    go (x, t) th = case Map.lookup x th of
      Nothing -> return $ Map.insert x t th
      Just u -> do
        th' <- unify t u
        combine th' th

type TCMonad m =
  (FreshFree m, FreshHole m, MonadFail m, MonadReader (Module Void) m)

-- TODO: move holeCtxs to Monad
-- TODO: implement as a cataExprM?
infer :: TCMonad m => Term Unit -> m (Ann Type ('Term HoleCtx), Unify)
infer expr = do
  m <- ask
  let cs = Map.fromList $ ctrs m
  let fs = Map.fromList $ sigs m
  (e, th) <- (Map.empty &) $ expr & cataExpr \e loc -> case e of
    Hole _ -> do
      g <- Var <$> fresh
      return (Hole (HoleCtx g loc) `Annot` g, Map.empty)
    Ctr c -> do
      t <- failMaybe $ Map.lookup c cs
      u <- instantiateFresh t
      return (Annot (Ctr c) u, Map.empty)
    Var a | Just t <- Map.lookup a loc ->
      return (Var a `Annot` t, Map.empty)
    Var a -> case Map.lookup a fs of
      Nothing -> fail $ "Variable not in scope: " <> show a
      Just t -> do
        u <- instantiateFresh t
        return (Var a `Annot` u, Map.empty)
    App f x -> do
      (f'@(Annot _ a), th1) <- f loc
      (x'@(Annot _ b), th2) <- x loc
      t <- Var <$> fresh
      th3 <- unify (subst th2 a) (Arr b t)
      let th4 = th3 `compose` th2 `compose` th1
      return (App f' x' `Annot` subst th4 t, th4)
    Lam a x -> do
      t <- Var <$> fresh
      (x'@(Annot _ u), th) <- x (Map.insert a t loc)
      let t' = subst th t
      return (Lam a x' `Annot` Arr t' u, th)
    Let a x y -> do
      t <- Var <$> fresh
      (x'@(Annot _ t1), th1) <- x loc
      (y'@(Annot _ t2), th2) <- y (Map.insert a t loc)
      let th3 = th2 `compose` th1
      th4 <- unify (subst th3 t) t1
      let th5 = th4 `compose` th3
      return (Let a x' y' `Annot` subst th5 t2, th5)
    Elim xs -> do
      t <- Var <$> fresh
      u <- Var <$> fresh
      (ys, t', u', th') <-
        xs & flip foldr (return ([], t, u, mempty)) \(c, y) r -> do
        (as, t1, u1, th1) <- r
        d <- failMaybe $ Map.lookup c cs
        Args args res <- instantiateFresh d
        (y'@(Annot _ t2), th2) <- y loc
        -- Check that the constructor type matches the scrutinee.
        th3 <- unify res t1
        -- Check that the branch type matches the resulting type.
        th4 <- unify (foldr Arr u1 args) t2
        let th5 = th4 `compose` th3 `compose` th2 `compose` th1
        return ((c, y'):as, subst th5 t1, subst th5 u1, th5)
      return (Elim (reverse ys) `Annot` Arr t' u', th')
    Fix -> do
      t <- Var <$> fresh
      return (Fix `Annot` Arr (Arr t t) t, mempty)

  -- TODO: do we need to subst th over local environments?
  return (mapAnn (subst th) e, th)

-- TODO: perhaps we should allow `Ann (Maybe Type) 'Term Var Unit` as input, so
-- partially annotated expressions.
check :: TCMonad m => Term Unit -> Poly ->
  m (Ann Type ('Term HoleCtx), Unify)
check e p = do
  (e'@(Annot _ u), th1) <- infer e
  th2 <- unify (freeze p) u
  let th3 = compose th2 th1
  let substCtx (HoleCtx g vs) = HoleCtx (subst th3 g) (subst th3 <$> vs)
  return (over holesAnn substCtx $ mapAnn (subst th3) e', th3)

check' :: TCMonad m => Term Unit -> Poly ->
  m (Ann Type ('Term Hole), Unify, Map Hole HoleCtx)
check' e t = do
  (x, th) <- check e t
  y <- forOf holesAnn x \ctx -> (,ctx) <$> fresh
  return (over holesAnn fst y, th, Map.fromList $ toListOf holesAnn y)
