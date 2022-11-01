{-# LANGUAGE GADTs, MultiParamTypeClasses #-}
module Language.Type
  ( Infer
  , runInfer, evalInfer
  , unify, unifies
  , infer, check
  , freshHoles
  ) where

import Import
import Language.Syntax
import qualified RIO.Map as Map

-- Type checking state {{{

type Infer = RWST Env () (Fresh Free) Maybe

runInfer :: Infer a -> Fresh Free -> Env -> Maybe (a, Fresh Free)
runInfer tc fr m = do
  (x, s, _) <- runRWST tc m fr
  return (x, s)

evalInfer :: Infer a -> Fresh Free -> Env -> Maybe a
evalInfer tc fr m = fst <$> runInfer tc fr m

-- }}}

-- | Type unification & inference

type Unify = Map Free Type

-- | Unify two monotypes, by checking if their holes can be filled such that
-- they are equivalent.
unify :: Type -> Type -> Maybe Unify
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
unifies :: Foldable t => t (Type, Type) -> Maybe Unify
unifies = flip foldr (return Map.empty) \(t1, t2) th -> do
  th0 <- th
  th1 <- unify (subst th0 t1) (subst th0 t2)
  return $ compose th1 th0

-- -- | Try to combine possibly conflicting unifications.
-- combine :: Unify -> Unify -> Maybe Unify
-- combine th1 th2 = foldr (\y z -> z >>= go y)
--   (return $ subst th1 <$> th2) $ Map.assocs th1 where
--     go (x, t) th = case Map.lookup x th of
--       Nothing -> return $ Map.insert x t th
--       Just u -> do
--         th' <- unify t u
--         combine th' th

-- TODO: move holeCtxs to Monad
-- TODO: implement as a cataExprM?
infer :: Map Var Poly -> Term h ->
  Infer (Ann Type ('Term (h, HoleCtx)), Unify)
infer ts expr = do
  cs <- view constructors
  fs <- view functions
  (e, th) <- (ts &) $ expr & cataExpr \e loc -> case e of
    Hole h -> do
      g <- Var <$> getFresh
      return (Hole (h, HoleCtx g loc) `Annot` g, Map.empty)
    Ctr c -> do
      t <- failMaybe $ Map.lookup c cs
      u <- instantiateFresh_ t
      return (Annot (Ctr c) u, Map.empty)
    Var a | Just p <- Map.lookup a loc -> do
      t <- instantiateFresh_ p
      return (Var a `Annot` t, Map.empty)
    Var a -> case Map.lookup a fs of
      Nothing -> fail $ "Variable not in scope: " <> show a
      Just t -> do
        u <- instantiateFresh_ t
        return (Var a `Annot` u, Map.empty)
    App f x -> do
      (f'@(Annot _ a), th1) <- f loc
      (x'@(Annot _ b), th2) <- x (subst th1 <$> loc)
      t <- Var <$> getFresh
      let th3 = th2 `compose` th1
      th4 <- failMaybe $ unify (subst th3 a) (Arr b t)
      let th5 = th4 `compose` th3
      return (App f' x' `Annot` subst th5 t, th5)
    Lam a x -> do
      t <- Var <$> getFresh
      (x'@(Annot _ u), th) <- x (Map.insert a (Mono t) loc)
      let t' = subst th t
      return (Lam a x' `Annot` Arr t' u, th)
    Let a x y -> do -- TODO: add let polymorphism
      (x'@(Annot _ t1), th1) <- x loc
      let loc' = Map.insert a (poly t1) $ subst th1 <$> loc
      (y'@(Annot _ t2), th2) <- y loc'
      return (Let a x' y' `Annot` t2, th2 `compose` th1)
    Elim xs -> do
      t <- Var <$> getFresh
      u <- Var <$> getFresh
      (ys, t', u', th') <-
        xs & flip foldr (return ([], t, u, mempty)) \(c, y) r -> do
        (as, t1, u1, th1) <- r
        d <- failMaybe $ Map.lookup c cs
        Args args res <- instantiateFresh_ d
        (y'@(Annot _ t2), th2) <- y (subst th1 <$> loc)
        -- Check that the constructor type matches the scrutinee.
        th3 <- failMaybe $ unify res t1
        -- Check that the branch type matches the resulting type.
        th4 <- failMaybe $ unify (foldr Arr u1 args) t2
        let th5 = th4 `compose` th3 `compose` th2 `compose` th1
        return ((c, y'):as, subst th5 t1, subst th5 u1, th5)
      return (Elim (reverse ys) `Annot` Arr t' u', th')
    Fix -> do
      t <- Var <$> getFresh
      return (Fix `Annot` Arr (Arr t t) t, mempty)
  return (over (holesAnn . _2) (subst th) . mapAnn (subst th) $ e, th)

check :: Map Var Poly -> Term h -> Poly ->
  Infer (Ann Type ('Term (h, HoleCtx)), Unify)
check ts e p = do
  (e'@(Annot _ u), th1) <- infer ts e
  th2 <- failMaybe $ unify (freeze p) u
  let th3 = compose th2 th1
  return (over (holesAnn . _2) (subst th3) . mapAnn (subst th3) $ e', th3)
