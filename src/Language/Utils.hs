{-# LANGUAGE RankNTypes #-}
module Language.Utils where

import Import
import Language.Syntax
import Data.Foldable
import qualified RIO.Map as Map

-- * Utility functions

tApps :: NonEmpty Type -> Type
tApps = foldl1 TApp

eApps :: NonEmpty (Expr a) -> (Expr a)
eApps = foldl1 EApp

tArrs :: NonEmpty Type -> Type
tArrs = foldr1 TArr

mkEnv :: [Binding] -> Env
mkEnv = foldr (\(Binding (Bound x) t) -> Map.insert x t) Map.empty

-- | Generates all possible ways to apply holes to an expression
expand :: Hole -> Sketch -> Type -> [(Sketch, Type)]
expand n sketch@(Sketch e ts) t = (sketch, t) : case t of
  TArr t1 t2 ->
    expand (1 + n) (Sketch (EApp e (EHole n)) (Map.insert n t1 ts)) t2
  _ -> []

-- | For each function signature, we compute all possible ways it can be
-- applied to holes.
instantiations :: Env -> Map Text [(Sketch, Type)]
instantiations = Map.mapWithKey \s t ->
  expand 0 (Sketch (EVar (Left (Bound s))) mempty) t

holeContexts :: Env -> Expr Hole -> Map Hole Env
holeContexts env = \case
  ELam (Binding (Bound x) t) e -> holeContexts (Map.insert x t env) e
  EApp x y -> Map.unionsWith Map.intersection
    [ holeContexts env x
    , holeContexts env y
    ]
  EHole i -> Map.singleton i env
  _ -> Map.empty

