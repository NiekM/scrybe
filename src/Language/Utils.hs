{-# LANGUAGE RankNTypes #-}
module Language.Utils where

import Import
import Language.Syntax
import qualified RIO.Map as Map

-- * Utility functions

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

holeContexts :: Env -> Expr -> Map Hole Env
holeContexts env = \case
  ELam (Bound x) t e -> holeContexts (Map.insert x t env) e
  EApp x y -> Map.unionsWith Map.intersection
    [ holeContexts env x
    , holeContexts env y
    ]
  EHole i -> Map.singleton i env
  _ -> Map.empty

