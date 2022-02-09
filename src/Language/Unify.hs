{-# LANGUAGE GADTs #-}
module Language.Unify where

import Import
import Language.Syntax
import RIO.List
import qualified RIO.Map as Map
import qualified RIO.Set as Set

free :: (Ord a, Foldable m) => m a -> Set a
free = Set.fromList . toList

-- | Unify two expressions, by checking if their holes can be filled such that
-- they are equivalent.
unify :: Ord a => Expr l a -> Expr l a -> Maybe (Map a (Expr l a))
unify t u = case (t, u) of
  (App t1 t2, App u1 u2) -> unifies [(t1, u1), (t2, u2)]
  (Var  a, Var  b) | a == b -> return Map.empty
  (Ctr  a, Ctr  b) | a == b -> return Map.empty
  (Hole a, Hole b) | a == b -> return Map.empty
  (Hole a, _) | occursCheck a u -> return $ Map.singleton a u
  (_, Hole a) | occursCheck a t -> return $ Map.singleton a t
  -- For sake of completeness, but not sure if unification makes any sense for
  -- term level expressions.
  (Lam x t1, Lam y u1) | x == y -> unify t1 u1
  (Case xs, Case ys) ->
    let xs' = sortOn pat xs
        ys' = sortOn pat ys
    in do
      guard $ ((==) `on` fmap pat) xs' ys'
      unifies $ zipWith (\x y -> (arm x, arm y)) xs' ys'
  _ -> Nothing
  where
    occursCheck :: Ord a => a -> Expr l a -> Bool
    occursCheck a tau = a `notElem` free tau

    unifies :: Ord a => [(Expr l a, Expr l a)] -> Maybe (Map a (Expr l a))
    unifies = flip foldr (return Map.empty) \(t1, t2) th -> do
      th0 <- th
      th1 <- unify (subst th0 t1) (subst th0 t2)
      return $ compose th0 th1
