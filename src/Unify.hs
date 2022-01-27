module Unify where

import Import
import Lang
import Subst
import RIO.List (nub)
import Data.Generics.Uniplate.Data (universeBi)
import qualified RIO.Map as Map

-- | Returns all free variables in a list, in the order of their occurence
free :: Type -> [TFree]
free = nub . universeBi

unify :: Type -> Type -> Maybe (Map TFree Type)
unify t u = case (t, u) of
  (TApp t1 t2, TApp u1 u2) -> unifies [(t1, u1), (t2, u2)]
  (TVar a, TVar b) | a == b -> return Map.empty
  (TVar (Right a), _) | occursCheck a u -> return $ Map.singleton a u
  (_, TVar (Right a)) | occursCheck a t -> return $ Map.singleton a t
  _ -> Nothing
  where
    occursCheck :: TFree -> Type -> Bool
    occursCheck a tau = a `notElem` free tau

    unifies :: [(Type, Type)] -> Maybe (Map TFree Type)
    unifies = flip foldr (return Map.empty) \(t1, t2) th -> do
      th0 <- th
      th1 <- unify (subst th0 t1) (subst th0 t2)
      return $ compose th0 th1

