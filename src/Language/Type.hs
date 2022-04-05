{-# LANGUAGE GADTs #-}
module Language.Type where

import Import
import Language.Syntax
import Language.Utils
import qualified RIO.Map as Map

type Unify l v h = Map v (Expr l v h)

-- TODO: Add unit tests to test type unification, inference and checking

-- | Unify two expressions, by checking if their holes can be filled such that
-- they are equivalent.
unify :: (Ord v, Eq h, MonadFail m, HasVar l, NoBind l, expr ~ Expr l v h) =>
  expr -> expr -> m (Map v expr)
unify t u = case (t, u) of
  (App t1 t2, App u1 u2) -> unifies [(t1, u1), (t2, u2)]
  (Var  a, Var  b) | a == b -> return Map.empty
  (Ctr  a, Ctr  b) | a == b -> return Map.empty
  (Hole a, Hole b) | a == b -> return Map.empty
  (Var a, _) | occurs a u -> return $ Map.singleton a u
  (_, Var a) | occurs a t -> return $ Map.singleton a t
  _ -> fail "Unification failed"

occurs :: (Eq v, HasVar l, NoBind l) => v -> Expr l v h -> Bool
occurs a tau = a `notElem` toListOf free tau

-- NOTE: it seems that the left hand side of the composition should be the
-- newer composition, in effect updating the old substitution according to the
-- new ones
-- | Compose two non-conflicting unifications.
compose :: (Ord v, th ~ Unify l v h, NoBind l) => th -> th -> th
compose sigma gamma = Map.unions
  [ subst sigma <$> gamma
  , Map.withoutKeys sigma (Map.keysSet gamma)
  ]

-- | Unify multiple expressions.
unifies :: (Ord v, Eq h, MonadFail m, Foldable t, HasVar l, NoBind l) =>
  expr ~ Expr l v h => t (expr, expr) -> m (Map v expr)
unifies = flip foldr (return Map.empty) \(t1, t2) th -> do
  th0 <- th
  th1 <- unify (subst th0 t1) (subst th0 t2)
  return $ compose th1 th0

-- | Try to combine possibly conflicting unifications.
combine :: (Ord v, Eq h, MonadFail m, HasVar l, NoBind l, th ~ Unify l v h) =>
  th -> th -> m th
combine th1 th2 = foldr (\y z -> z >>= go y)
  (return $ subst th1 <$> th2) $ Map.assocs th1 where
    go (x, t) th = case Map.lookup x th of
      Nothing -> return $ Map.insert x t th
      Just u -> do
        th' <- unify t u
        combine th' th

-- TODO: move holeCtxs to Monad
-- TODO: implement as a catamorphism?
infer :: (FreshFree m, FreshVarId m, FreshHole m, MonadFail m) =>
  (MonadReader (Module Void) m, WithVariables s m) => Term Var Unit ->
  m (Ann Type 'Term Var Hole, Unify 'Type Var Void, Map Hole HoleCtx)
infer expr = do
  m <- ask
  let cs = ctrs m
  let fs = functions m
  let go loc = \case
        Hole _ -> do
          h <- fresh
          g <- Var . freeId <$> fresh
          -- Note variable availability
          modifying variables $ Map.mapWithKey \x -> \case
            Variable name t i n | x `elem` loc -> Variable name t (i + 1) n
            v -> v
          return
            (Hole h `Annot` g, Map.empty, Map.singleton h (HoleCtx g loc))
        Ctr c -> do
          t <- failMaybe $ Map.lookup c cs
          u <- instantiateFresh t
          return (Annot (Ctr c) u, Map.empty, Map.empty)
        Var a | Just x <- Map.lookup a loc -> use variables >>= \vs ->
          case Map.lookup x vs of
            Nothing -> fail $ "Missing variable id " <> show x
            Just (Variable name t i n) -> do
              -- Note the variable occurrence
              modifying variables . Map.insert x $ Variable name t i (n + 1)
              return (Var a `Annot` t, Map.empty, Map.empty)
        Var a -> do
          (_, t) <- failMaybe $ Map.lookup a fs
          u <- instantiateFresh t
          return (Var a `Annot` u, Map.empty, Map.empty)
        App f x -> do
          (f'@(Annot _ a), th1, ctx1) <- go loc f
          (x'@(Annot _ b), th2, ctx2) <- go loc x
          t <- Var . freeId <$> fresh
          th3 <- unify (subst th2 a) (Arr b t)
          let th4 = th3 `compose` th2 `compose` th1
          let ctx3 = over goal (subst th4) <$> ctx1 <> ctx2
          modifying variables . fmap $ over varType (subst th4)
          return (App f' x' `Annot` subst th4 t, th4, ctx3)
        Lam x e -> do
          t <- Var . freeId <$> fresh
          i <- fresh
          modifying variables $ Map.insert i (Variable x t 1 0)
          (e'@(Annot _ u), th, local') <- go (Map.insert x i loc) e
          let t' = subst th t
          return (Lam x e' `Annot` Arr t' u, th, local')
        Let {} -> undefined
        Case {} -> undefined
  (e, th, ctx) <- go Map.empty expr
  return (mapAnn (subst th) e, th, ctx)

-- TODO: perhaps we should allow `Ann (Maybe Type) 'Term Var Unit` as input, so
-- partially annotated expressions.
check :: (FreshFree m, FreshHole m, FreshVarId m, MonadFail m,
  MonadReader (Module Void) m, WithVariables s m) => Term Var Unit -> Poly ->
  m (Ann Type 'Term Var Hole, Unify 'Type Var Void, Map Hole HoleCtx)
check e p = do
  (e'@(Annot _ u), th1, ctx1) <- infer e
  th2 <- unify (freeze p) u
  let th3 = compose th2 th1
  let ctx2 = over goal (subst th3) <$> ctx1
  modifying variables . fmap $ over varType (subst th3)
  return (mapAnn (subst th3) e', th3, ctx2)
