module Algorithms.TypeIndexed where

import Import
import Language.Syntax
import Language.Utils
import qualified RIO.Map as Map

{-

Type-indexed searching

When searching for expressions adhering to a concrete type t, i.e. without any
holes, rather than trying to unify t with the type of each expression in our
search space, we can make our search space type-indexed. By `punching' holes in
type t, we can compute all expressions that would unify with t, as well as the
corresponding unification results. By also normalizing the types in our search
space, finding all expressions whose type would unify with type t is very
simple.

Unfortunately, this does not yet work when type t itself has holes, which is
quite common during synthesis. As such, this method might not be very useful.
However, the normalization of types in the search space is still benefitial, as
it allows expressions with the same type to be grouped together, so that we
don't have to perform the same unification twice.

-}

-- | Lookup all values in a map whose keys unify with the lookup value.
lookup :: Expr l Void -> Map (Expr l Hole) a -> [(a, Map Hole (Expr l Void))]
lookup t m = Map.elems $ Map.intersectionWith (,) m (generalize t)

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
