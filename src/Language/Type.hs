module Language.Type
  ( Unify, Infer
  , runInfer, evalInfer
  , unify, unifies
  , infer, check, checks
  ) where

import Import
import Language.Syntax
import qualified RIO.Map as Map

-- || Type checking state

type Infer = RWST Env () (Fresh Free) Maybe

runInfer :: Infer a -> Fresh Free -> Env -> Maybe (a, Fresh Free)
runInfer tc fr m = do
  (x, s, _) <- runRWST tc m fr
  return (x, s)

evalInfer :: Infer a -> Fresh Free -> Env -> Maybe a
evalInfer tc fr m = fst <$> runInfer tc fr m

-- || Type unification

type Unify = Map Free Mono

-- | Unify two monotypes, by checking if their holes can be filled such that
-- they are equivalent.
unify :: Mono -> Mono -> Maybe Unify
unify t u = case (t, u) of
  (Arr t1 t2, Arr u1 u2) -> unifies [(t1, u1), (t2, u2)]
  (Var  a, Var  b) | a == b -> return Map.empty
  (Ctr c xs, Ctr d ys) | c == d, length xs == length ys -> unifies (zip xs ys)
  (Var a, _) | occurs a u -> return $ Map.singleton a u
  (_, Var a) | occurs a t -> return $ Map.singleton a t
  _ -> fail "Unification failed"

-- | Occurs check.
occurs :: Free -> Mono -> Bool
occurs a tau = a `notElem` toListOf free tau

-- | Compose two non-conflicting unifications.
compose :: Unify -> Unify -> Unify
compose sigma gamma = Map.unions
  [ subst sigma <$> gamma
  , Map.withoutKeys sigma (Map.keysSet gamma)
  ]

-- | Unify multiple expressions.
unifies :: Foldable t => t (Mono, Mono) -> Maybe Unify
unifies = flip foldr (return Map.empty) \(t1, t2) th -> do
  th0 <- th
  th1 <- unify (subst th0 t1) (subst th0 t2)
  return $ compose th1 th0

-- || Type inference and checking.

-- | Type inference.
infer :: Map Var Poly -> Term h -> Infer (Mono, Term Goal)
infer ts expr = do
  cs <- view envConstructors
  fs <- view envFunctions
  (t, e, th) <- (ts &) $ expr & cataExpr \e loc -> case e of
    Hole _ -> do
      g <- Var <$> fresh
      return (g, Hole (Goal loc g), Map.empty)
    Ctr c xs -> do
      t <- failMaybe $ Map.lookup c cs
      Args us u <- instantiateFresh t
      (vs, ys, th) <-
        xs & flip foldr (return ([], [], mempty)) \x r -> do
          (us', xs', th1) <- r
          (u', x', th2) <- x (subst th1 <$> loc)
          let th3 = th2 `compose` th1
          return (u' : us', x' : xs', th3)
      th' <- failMaybe . unifies $ zip (subst th <$> us) vs
      let th'' = th' `compose` th
      return (subst th'' u, Ctr c ys, th'')
    Var a | Just p <- Map.lookup a loc -> do
      t <- instantiateFresh p
      return (t, Var a, Map.empty)
    Var a -> case Map.lookup a fs of
      Nothing -> fail $ "Variable not in scope: " <> show a
      Just t -> do
        u <- instantiateFresh t
        return (u, Var a, Map.empty)
    App f x -> do
      (a, f', th1) <- f loc
      (b, x', th2) <- x (subst th1 <$> loc)
      t <- Var <$> fresh
      let th3 = th2 `compose` th1
      th4 <- failMaybe $ unify (subst th3 a) (Arr b t)
      let th5 = th4 `compose` th3
      return (subst th5 t, App f' x', th5)
    Lam a x -> do
      t <- Var <$> fresh
      (u, x', th) <- x (Map.insert a (Mono t) loc)
      let t' = subst th t
      return (Arr t' u, Lam a x', th)
    Let a x y -> do -- TODO: add let polymorphism
      (t1, x', th1) <- x loc
      let loc' = Map.insert a (Mono t1) $ subst th1 <$> loc
      (t2, y', th2) <- y loc'
      return (t2, Let a x' y', th2 `compose` th1)
    Elim xs -> do
      t <- Var <$> fresh
      u <- Var <$> fresh
      (ys, t', u', th') <-
        xs & flip foldr (return ([], t, u, mempty)) \(c, y) r -> do
        (as, t1, u1, th1) <- r
        d <- failMaybe $ Map.lookup c cs
        Args args res <- instantiateFresh d
        (t2, y', th2) <- y (subst th1 <$> loc)
        -- Check that the constructor type matches the scrutinee.
        th3 <- failMaybe $ unify res t1
        -- Check that the branch type matches the resulting type.
        th4 <- failMaybe $ unify (foldr Arr u1 args) t2
        let th5 = th4 `compose` th3 `compose` th2 `compose` th1
        return ((c, y'):as, subst th5 t1, subst th5 u1, th5)
      return (Arr t' u', Elim (reverse ys), th')
    Fix -> do
      t <- Var <$> fresh
      return (Arr (Arr t t) t, Fix, mempty)
  return (subst th t, over holes (subst th) e)

-- | Type checking.
check :: Map Var Poly -> Term h -> Poly -> Infer (Mono, Term Goal, Unify)
check ts e p = do
  (u, e') <- infer ts e
  th <- failMaybe $ unify (freeze p) u
  return (subst th u, over holes (subst th) e', th)

checks :: Map Var Poly -> [(Term h, Mono)] -> Infer ([(Term Goal, Mono)], Unify)
checks ts = flip foldr (return ([], Map.empty)) \(e, t) r -> do
  (xs, th1) <- r
  (u, x, th2) <- check ts e (Mono $ subst th1 t)
  return ((x, u):xs, th1 `compose` th2)
