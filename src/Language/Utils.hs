module Language.Utils where

import Import hiding (reverse)
import Language.Syntax
import Data.Foldable
import qualified RIO.Map as Map
import Control.Monad.State

-- TODO: replace with more general infix function
arrs :: App l => NonEmpty (Expr l a) -> Expr l a
arrs = foldr1 Arr

-- | Return all holes in an expression.
holes :: Expr l a -> [a]
holes = toList

-- | Number all holes in an expression.
number :: (Num n, Traversable m) => m a -> State n (m (n, a))
number = traverse \x -> do
  n <- get
  put (n + 1)
  return (n, x)

-- | All ways to 'punch' holes into an expression without holes.
punch :: Expr l Void -> [Expr l (Expr l Void)]
punch = punch' . fmap absurd

-- | All ways to 'punch' holes into an expression, flattening any 'recursive'
-- holes.
punch' :: Expr l (Expr l a) -> [Expr l (Expr l a)]
punch' e = Hole (join e) : case e of
  Hole  _ -> []
  Var   x -> [Var x]
  Ctr _ _ -> undefined
  App f x -> App <$> punch' f <*> punch' x
  Lam b x -> Lam b <$> punch' x
  -- TODO: this is incorrect ...
  Case xs -> Case . sequenceA <$> (fmap punch' <$> xs)

-- | Replace all holes with numbers and return a mapping from numbers to the
-- initial hole values.
extract :: (Num k, Traversable m, Ord k) => m a -> (m k, Map k a)
extract t = fmap fst &&& Map.fromList . toList $
  flip evalState 0 $ number t

-- | Compute all possible ways to replace subexpressions with holes, along with
-- the hole fillings to reverse this.
generalize :: Expr l Void -> Map (Expr l Hole) (Map Hole (Expr l Void))
generalize = Map.fromList . fmap extract . punch

-- | All subexpressions, including the expression itself.
dissect :: Expr l a -> [Expr l a]
dissect e = e : case e of
  Hole _ -> []
  Var _ -> []
  Ctr _ xs -> concatMap dissect xs
  App f x -> dissect f ++ dissect x
  Lam _ x -> dissect x
  Case xs -> concatMap (dissect . arm) xs

-- | All possible ways to use an expression by applying it to a number of holes
expand :: App e => Expr e (Expr t a) -> Expr t a
  -> [(Expr e (Expr t a), Expr t a)]
expand e t = (e, t) : case t of
  Arr t1 t2 -> expand (App e (Hole t1)) t2
  _ -> []

-- | Compute the contexts for each hole in a sketch.
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
