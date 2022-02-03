module Language.Unify where

import Import
import Language.Syntax
import qualified RIO.Map as Map
import qualified RIO.Set as Set

free :: (Ord a, Foldable m) => m a -> Set a
free = Set.fromList . toList

unify :: Ord a => Type a -> Type a -> Maybe (Map a (Type a))
unify t u = case (t, u) of
  (TApp t1 t2, TApp u1 u2) -> unifies [(t1, u1), (t2, u2)]
  (TVar  a, TVar  b) | a == b -> return Map.empty
  (THole a, THole b) | a == b -> return Map.empty
  (THole a, _) | occursCheck a u -> return $ Map.singleton a u
  (_, THole a) | occursCheck a t -> return $ Map.singleton a t
  _ -> Nothing
  where
    occursCheck :: Ord a => a -> Type a -> Bool
    occursCheck a tau = a `notElem` free tau

    unifies :: Ord a => [(Type a, Type a)] -> Maybe (Map a (Type a))
    unifies = flip foldr (return Map.empty) \(t1, t2) th -> do
      th0 <- th
      th1 <- unify (subst th0 t1) (subst th0 t2)
      return $ compose th0 th1
