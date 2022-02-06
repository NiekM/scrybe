module Language.Utils where

import Import
import Language.Syntax
import Data.Foldable
-- import Data.Generics.Uniplate.Data
import qualified RIO.Map as Map
import Control.Monad.State

-- * Utility functions

apps :: NonEmpty (Expr l a) -> Expr l a
apps = foldl1 App

-- TODO: replace with more general infix function
arrs :: NonEmpty (Expr l a) -> Expr l a
arrs = foldr1 Arr

punch :: Expr l Void -> [Expr l (Expr l Void)]
punch = punch' . fmap absurd

punch' :: Expr l (Expr l a) -> [Expr l (Expr l a)]
punch' (Hole a) = [Hole a]
punch' e = Hole (join e) : case e of
  Hole  _ -> []
  Var   x -> [Var x]
  App f x -> App <$> punch' f <*> punch' x
  Lam b x -> Lam b <$> punch' x

-- | All subexpressions, including the expression itself.
dissect :: Expr l a -> [Expr l a]
dissect e = e : case e of
  Hole  _ -> []
  Var   _ -> []
  App f x -> dissect f ++ dissect x
  Lam _ x -> dissect x

expand :: Expr e (Expr t a) -> Expr t a -> [(Expr e (Expr t a), Expr t a)]
expand e t = (e, t) : case t of
  Arr t1 t2 -> expand (App e (Hole t1)) t2
  _ -> []

holeContexts :: Map Var (Type Hole) -> Term Hole
  -> Map Hole (Map Var (Type Hole))
holeContexts env = \case
  Lam (Bind x t) e -> holeContexts (Map.insert x t env) e
  App x y -> Map.unionsWith Map.intersection
    [ holeContexts env x
    , holeContexts env y
    ]
  Hole i -> Map.singleton i env
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
