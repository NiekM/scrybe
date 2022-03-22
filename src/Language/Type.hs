{-# LANGUAGE GADTs #-}
module Language.Type where

import Import
import Language.Syntax
import Language.Utils
import qualified RIO.Map as Map

type Unify l a = Map Var (Expr l a)

-- TODO: Add unit tests to test type unification, inference and checking

-- | Unify two expressions, by checking if their holes can be filled such that
-- they are equivalent.
unify :: (Eq b, MonadFail m, HasVar l, NoBind l, expr ~ Expr l b) =>
  expr -> expr -> m (Map (VAR l) expr)
unify t u = case (t, u) of
  (App t1 t2, App u1 u2) -> unifies [(t1, u1), (t2, u2)]
  (Var  a, Var  b) | a == b -> return Map.empty
  (Ctr  a, Ctr  b) | a == b -> return Map.empty
  (Hole a, Hole b) | a == b -> return Map.empty
  (Var a, _) | occurs a u -> return $ Map.singleton a u
  (_, Var a) | occurs a t -> return $ Map.singleton a t
  _ -> fail "Unification failed"

occurs :: (Eq (VAR l), HasVar l, NoBind l) => VAR l -> Expr l b -> Bool
occurs a tau = a `notElem` toListOf free tau

-- NOTE: it seems that the left hand side of the composition should be the
-- newer composition, in effect updating the old substitution according to the
-- new ones
compose :: (Ord (VAR l), th ~ Map (VAR l) (Expr l b)) => th -> th -> th
compose sigma gamma = Map.unions
  [ subst sigma <$> gamma
  , Map.withoutKeys sigma (Map.keysSet gamma)
  ]

unifies :: (Ord (VAR l), Eq b, MonadFail m, Foldable t, HasVar l, NoBind l) =>
  expr ~ Expr l b => t (expr, expr) -> m (Map (VAR l) expr)
unifies = flip foldr (return Map.empty) \(t1, t2) th -> do
  th0 <- th
  th1 <- unify (subst th0 t1) (subst th0 t2)
  return $ compose th1 th0

-- TODO: move holeCtxs to Monad
infer :: (FreshFree m, FreshVarId m, MonadFail m, MonadReader (Module Void) m
         , WithVariables s m) =>
  Term Hole -> m (Type Void, Unify 'Type Void, Map Hole HoleCtx)
infer expr = do
  m <- ask
  let cs = ctrs m
  let fs = functions m
  let go loc = \case
        Hole h -> do
          g <- Var . freeId <$> fresh
          -- Note variable availability
          modifying variables $ Map.mapWithKey \x -> \case
            Variable name t i n | x `elem` loc -> Variable name t (i + 1) n
            v -> v
          return (g, Map.empty, Map.singleton h (HoleCtx g loc))
        Ctr c -> do
          t <- failMaybe $ Map.lookup c cs
          u <- instantiateFresh t
          return (u, Map.empty, Map.empty)
        Var a | Just x <- Map.lookup a loc -> use variables >>= \vs ->
          case Map.lookup x vs of
            Nothing -> fail $ "Missing variable id " <> show x
            Just (Variable name t i n) -> do
              -- Note the variable occurrence
              modifying variables . Map.insert x $ Variable name t i (n + 1)
              return (t, Map.empty, Map.empty)
        Var a -> do
          (_, t) <- failMaybe $ Map.lookup a fs
          u <- instantiateFresh t
          return (u, Map.empty, Map.empty)
        App f x -> do
          (a, th1, ctx1) <- go loc f
          (b, th2, ctx2) <- go loc x
          t <- Var . freeId <$> fresh
          th3 <- unify (subst th2 a) (Arr b t)
          let th4 = th3 `compose` th2 `compose` th1
          let ctx3 = over goal (subst th4) <$> ctx1 <> ctx2
          modifying variables .fmap $ over varType (subst th4)
          return (subst th4 t, th4, ctx3)
        Lam x e -> do
          t <- Var . freeId <$> fresh
          i <- fresh
          (u, th, local') <- go (Map.insert x i loc) e
          let t' = subst th t
          modifying variables $ Map.insert i (Variable x t' 1 0)
          return (Arr t' u, th, local')
        Let {} -> undefined
        Case {} -> undefined
  go Map.empty expr

-- TODO: maybe this should return a sketch along with a type and unification
check :: (FreshFree m, FreshHole m, FreshVarId m, MonadFail m,
  MonadReader (Module Void) m, WithVariables s m) => Term Unit -> Type Void ->
  m (Term Hole, Type Void, Unify 'Type Void, Map Hole HoleCtx)
check e t = do
  e' <- traverseOf holes (const fresh) e
  (u, th1, ctx1) <- infer e'
  th2 <- unify t u
  let th3 = compose th2 th1
  let ctx2 = over goal (subst th3) <$> ctx1
  modifying variables . fmap $ over varType (subst th3)
  return (e', subst th3 u, th3, ctx2)
