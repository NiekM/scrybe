{-# LANGUAGE GADTs, MultiParamTypeClasses #-}
module Language.Type where

import Import
import Language.Syntax
import qualified RIO.Map as Map

-- Type checking state {{{

data TCState = TCState
  { _locals     :: Map VarId Variable
  , _freshState :: FreshState
  }

class HasTCState a where
  tcState :: Lens' a TCState

instance HasVars TCState where
  variables = lens _locals \x y -> x { _locals = y }

instance HasFreshState TCState where
  freshState = lens _freshState \x y -> x { _freshState = y }

mkTCState :: TCState
mkTCState = TCState mempty mkFreshState

runTC :: Monad m => RWST (Module Void) () TCState m a ->
  Module Void -> m (a, TCState)
runTC tc m = do
  (x, s, _) <- runRWST tc m mkTCState
  return (x, s)

evalTC :: Monad m => RWST (Module Void) () TCState m a -> Module Void -> m a
evalTC tc m = fst <$> runTC tc m

-- }}}

-- | Type unification & inference

type Unify = Map Var Type

-- TODO: Add unit tests to test type unification, inference and checking

-- | Unify two expressions, by checking if their holes can be filled such that
-- they are equivalent.
unify :: MonadFail m => Type -> Type -> m (Map Var Type)
unify t u = case (t, u) of
  (App t1 t2, App u1 u2) -> unifies [(t1, u1), (t2, u2)]
  (Var  a, Var  b) | a == b -> return Map.empty
  (Ctr  a, Ctr  b) | a == b -> return Map.empty
  (Hole a, Hole b) | a == b -> return Map.empty
  (Var a, _) | occurs a u -> return $ Map.singleton a u
  (_, Var a) | occurs a t -> return $ Map.singleton a t
  _ -> fail "Unification failed"

occurs :: Var -> Type -> Bool
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
unifies :: (MonadFail m, Foldable t) => t (Type, Type) -> m (Map Var Type)
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

-- TODO: move holeCtxs to Monad
-- TODO: implement as a cataExprM?
infer :: (FreshFree m, FreshVarId m, FreshHole m, MonadFail m) =>
  (MonadReader (Module Void) m, MonadState s m, HasVars s) =>
  Term Var Unit -> m (Ann Type 'Term Var Hole, Unify, Map Hole HoleCtx)
infer expr = do
  m <- ask
  let cs = Map.fromList $ ctrs m
  let fs = Map.fromList $ sigs m
  (e, th, ctx) <- (Map.empty &) $ expr & cataExpr \e loc -> case e of
    Hole _ -> do
      h <- fresh
      g <- Var . freeId <$> fresh
      -- Note variable availability
      modifying variables $ Map.mapWithKey \x -> \case
        Variable name t i n | x `elem` loc -> Variable name t (i + 1) n
        v -> v
      return (Hole h `Annot` g, Map.empty, Map.singleton h (HoleCtx g loc))
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
      t <- failMaybe $ Map.lookup a fs
      u <- instantiateFresh t
      return (Var a `Annot` u, Map.empty, Map.empty)
    App f x -> do
      (f'@(Annot _ a), th1, ctx1) <- f loc
      (x'@(Annot _ b), th2, ctx2) <- x loc
      t <- Var . freeId <$> fresh
      th3 <- unify (subst th2 a) (Arr b t)
      let th4 = th3 `compose` th2 `compose` th1
      let ctx3 = over goal (subst th4) <$> ctx1 <> ctx2
      modifying variables . fmap $ over varType (subst th4)
      return (App f' x' `Annot` subst th4 t, th4, ctx3)
    Lam a x -> do
      t <- Var . freeId <$> fresh
      i <- fresh
      modifying variables $ Map.insert i (Variable a t 1 0)
      (x'@(Annot _ u), th, local') <- x (Map.insert a i loc)
      let t' = subst th t
      return (Lam a x' `Annot` Arr t' u, th, local')
    Let a x y -> do
      t <- Var . freeId <$> fresh
      i <- fresh
      modifying variables $ Map.insert i (Variable a t 1 0)
      (x'@(Annot _ t1), th1, ctx1) <- x loc
      (y'@(Annot _ t2), th2, ctx2) <- y (Map.insert a i loc)
      let th3 = th2 `compose` th1
      th4 <- unify (subst th3 t) t1
      let th5 = th4 `compose` th3
      let ctx3 = over goal (subst th5) <$> ctx1 <> ctx2
      modifying variables . fmap $ over varType (subst th5)
      return (Let a x' y' `Annot` subst th5 t2, th5, ctx3)
    Elim xs -> do
      t <- Var . freeId <$> fresh
      u <- Var . freeId <$> fresh
      (ys, t', u', th', ctx') <-
        xs & flip foldr (return ([], t, u, mempty, mempty)) \(c, y) r -> do
        (as, t1, u1, th1, ctx1) <- r
        d <- failMaybe $ Map.lookup c cs
        Args args res <- instantiateFresh d
        (y'@(Annot _ t2), th2, ctx2) <- y loc
        -- Check that the constructor type matches the scrutinee.
        th3 <- unify res t1
        -- Check that the branch type matches the resulting type.
        th4 <- unify (foldr Arr u1 args) t2
        let th5 = th4 `compose` th3 `compose` th2 `compose` th1
        let ctx3 = over goal (subst th5) <$> ctx1 <> ctx2
        modifying variables . fmap $ over varType (subst th5)
        return ((c, y'):as, subst th5 t1, subst th5 u1, th5, ctx3)
      return (Elim (reverse ys) `Annot` Arr t' u', th', ctx')
    Fix -> do
      t <- Var . freeId <$> fresh
      return (Fix `Annot` Arr (Arr t t) t, mempty, mempty)

  return (mapAnn (subst th) e, th, ctx)

-- TODO: perhaps we should allow `Ann (Maybe Type) 'Term Var Unit` as input, so
-- partially annotated expressions.
check :: (FreshFree m, FreshHole m, FreshVarId m, MonadFail m) =>
  (MonadReader (Module Void) m, MonadState s m, HasVars s) =>
  Term Var Unit -> Poly -> m (Ann Type 'Term Var Hole, Unify, Map Hole HoleCtx)
check e p = do
  (e'@(Annot _ u), th1, ctx1) <- infer e
  th2 <- unify (freeze p) u
  let th3 = compose th2 th1
  let ctx2 = over goal (subst th3) <$> ctx1
  modifying variables . fmap $ over varType (subst th3)
  return (mapAnn (subst th3) e', th3, ctx2)
