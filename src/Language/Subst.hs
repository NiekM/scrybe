{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Language.Subst where

import Language.Syntax

import Import
import Data.Generics.Uniplate.Data (transformBi)
import qualified RIO.Map as Map

class Extract a b where
  extract :: a -> Maybe b

instance Extract Type Free where
  extract = \case
    TVar (Right a) -> Just a
    _ -> Nothing

instance Extract (Expr a) a where
  extract = \case
    EHole a -> Just a
    _ -> Nothing

pattern Extract :: Extract a b => b -> a
pattern Extract a <- (extract -> Just a)

subst :: (Ord k, Data a, Data v, Extract v k) => Map k v -> a -> a
subst th = transformBi \case
  Extract a | Just x <- Map.lookup a th -> x
  a -> a

compose :: (Data v, Data k, Ord k, Extract v k)
        => Map k v -> Map k v -> Map k v
compose sigma gamma = Map.unions
  [ subst sigma gamma
  , Map.withoutKeys sigma (Map.keysSet gamma)
  ]

