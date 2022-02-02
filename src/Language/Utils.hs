{-# LANGUAGE EmptyCase #-}
module Language.Utils where

import Import
import Language.Syntax
import Data.Foldable
import Data.Generics.Uniplate.Data
import qualified RIO.Map as Map
import Control.Monad.State

-- * Utility functions

tApps :: NonEmpty Type -> Type
tApps = foldl1 TApp

eApps :: NonEmpty (Expr a) -> Expr a
eApps = foldl1 EApp

tArrs :: NonEmpty Type -> Type
tArrs = foldr1 TArr

mkEnv :: [Binding] -> Env
mkEnv = foldr (\(Binding x t) -> Map.insert x t) Map.empty

-- | Generates all possible ways to apply holes to an expression.
expand :: Hole -> Sketch -> Type -> [(Sketch, Type)]
expand n sketch@(Sketch e ts) t = (sketch, t) : case t of
  TArr t1 t2 ->
    expand (1 + n) (Sketch (EApp e (EHole n)) (Map.insert n t1 ts)) t2
  _ -> []

-- | For each function signature, we compute all possible ways it can be
-- applied to holes.
instantiations :: Env -> Map Var [(Sketch, Type)]
instantiations = Map.mapWithKey \s t ->
  expand 0 (Sketch (EVar s) mempty) t

-- | Compute the contexts of every hole in a sketch.
holeContexts :: Env -> Expr Hole -> Map Hole Env
holeContexts env = \case
  ELam (Binding x t) e -> holeContexts (Map.insert x t env) e
  EApp x y -> Map.unionsWith Map.intersection
    [ holeContexts env x
    , holeContexts env y
    ]
  EHole i -> Map.singleton i env
  _ -> Map.empty

-- | Holes
holes :: Expr a -> [a]
holes = toList

-- | Renumber all holes in an expression.
renum :: Num n => Expr a -> State n (Expr n)
renum = traverse \_ -> do
  n <- get
  put (n + 1)
  return n

-- | All possible ways to `punch' holes into an expression, including zero
-- holes.
punch :: Expr Void -> [Expr (Expr Void)]
punch = punch' . fmap \case

punch' :: Data a => Expr (Expr a) -> [Expr (Expr a)]
punch' (EHole a) = pure (EHole a)
punch' e = pure (EHole (join e)) <|> descendM punch' e

-- | All subexpressions, including the expression itself.
dissect :: Data a => Expr a -> [Expr a]
dissect = universe
