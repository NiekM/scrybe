{-# LANGUAGE EmptyCase #-}
module Language.Utils where

import Import
import Language.Syntax
import Data.Foldable
import Data.Generics.Uniplate.Data
import qualified RIO.Map as Map
import Control.Monad.State

-- * Utility functions

tApps :: NonEmpty (Type a) -> Type a
tApps = foldl1 TApp

eApps :: NonEmpty (Expr a) -> Expr a
eApps = foldl1 EApp

tArrs :: NonEmpty (Type a) -> Type a
tArrs = foldr1 TArr

mkEnv :: [Binding a] -> Env a
mkEnv = foldr (\(Binding x t) -> Map.insert x t) Map.empty

xpnd :: Expr (Type a) -> Type a -> [(Expr (Type a), Type a)]
xpnd e t = (e, t) : case t of
  TArr t1 t2 -> xpnd (EApp e (EHole t1)) t2
  _ -> []

intsts :: Env a -> Map Var [(Expr (Type a), Type a)]
intsts = Map.mapWithKey (xpnd . EVar)

-- | Compute the contexts of every hole in a sketch.
holeContexts :: Env Hole -> Expr Hole -> Map Hole (Env Hole)
holeContexts env = \case
  ELam (Binding x t) e -> holeContexts (Map.insert x t env) e
  EApp x y -> Map.unionsWith Map.intersection
    [ holeContexts env x
    , holeContexts env y
    ]
  EHole i -> Map.singleton i env
  _ -> Map.empty

-- | Holes
holes :: Foldable m => m a -> [a]
holes = toList

-- | Number all holes in an expression.
number :: (Num n, Traversable m) => m a -> State n (m (n, a))
number = traverse \x -> do
  n <- get
  put (n + 1)
  return (n, x)

-- TODO: find a way to generalize these functions to both expressions and
-- types. Similarly for unification

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
