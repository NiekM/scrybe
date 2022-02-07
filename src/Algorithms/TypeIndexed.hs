module Algorithms.TypeIndexed where

import Import
import Language.Syntax
import Language.Utils
import qualified RIO.Map as Map
import Control.Monad.State

-- | Replace all holes with numbers and return a mapping from numbers to the
-- initial hole values.
extract :: (Num k, Traversable m, Ord k) => m a -> (m k, Map k a)
extract t = fmap fst &&& Map.fromList . toList $
  flip evalState 0 $ number t

-- | Compute all possible ways to replace subexpressions with holes, along with
-- the hole fillings to reverse this.
generalize :: Expr l Void -> Map (Expr l Hole) (Map Hole (Expr l Void))
generalize = Map.fromList . fmap extract . punch

-- | Lookup all values in a map whose keys unify with the lookup value.
lookup :: Expr l Void -> Map (Expr l Hole) a -> [(a, Map Hole (Expr l Void))]
lookup t m = Map.elems $ Map.intersectionWith (,) m (generalize t)

-- | Compute a type-indexed map from a module.
fromModule :: [Binding (Type Hole)] -> Map (Type Hole) [Term (Type Hole)]
fromModule env = Map.fromListWith (++) do
  Bind name t <- env
  (sk, ty) <- expand (Var name) t
  let (ty', sk') = normalize ty sk
  return (ty', [sk'])

-- | Normalize a type along with an expression with types in the holes, such
-- that the holes are numbered in order of their appearance.
normalize :: Type Hole -> Term (Type Hole) -> (Type Hole, Term (Type Hole))
normalize t e = (subst rename t, subst rename <$> e)
  where
    xs = holes t
    ys = concatMap holes (holes e)
    -- All holes in order of their appearance, given preference to holes in t
    zs = nubOrd $ xs ++ ys
    rename = Map.fromList $ zip zs (Hole . MkHole <$> [0..])
