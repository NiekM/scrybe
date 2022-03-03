{-# LANGUAGE RankNTypes #-}
module Language.Concepts where

import Import
import Language.Syntax
import Language.Type
import Language.Utils
import qualified RIO.Map as Map
import qualified RIO.Set as Set

-- TODO: keep everything in here abstract

-- NOTE: Just n means n occurences, Nothing means infinite occurences
-- TODO: perhaps rather than have 'infinite' occurences, just remove those
-- concepts from hole fillings
type Occur = Maybe Int
type MultiSet a = Map a Occur

fromSet :: Set a -> MultiSet a
fromSet = Map.fromSet . const . Just $ 1

reduce :: Occur -> Occur -> Maybe Occur
reduce Nothing (Just _) = Just Nothing
reduce (Just n) (Just k) | n > k = Just . Just $ n - k
reduce _ _ = Nothing

sub :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
sub a b = (<> Map.difference a b) . Map.mapMaybe id
  $ Map.intersectionWith reduce a b

-- TODO: how to handle different levels of precision/abstraction? for example,
-- we might have a concept for pattern matching and a more specific concept for
-- pattern matching on a concrete type. similarly for a function/datatype
-- concept vs that same function/datatype but with type parameters
-- instantiated. how do we handle these related concepts in a way that is sound
-- and makes sense?

-- TODO: 'naive' synthesis is actually point-free synthesis using a single
-- occurence for each concept. 'eta' synthesis is actually synthesis that keeps
-- the expression in eta-long form by allowing only eta expansion at function
-- types and disallowing partial application. this can all easily be captured
-- in environments, by seeing eta expansion as a hole filling. 'naive'
-- synthesis simply removes eta-expansions from the environment and 'eta'
-- synthesis removes partial applications (non eta-expansion hole fillings with
-- function types) from the environment. we can make 'eta-long' and
-- 'point-free' functions that add restrictions by transforming the
-- environment.

-- | Concepts within our language, used to categorize hole fillings.
data Concept
  = Function Var
  | Datatype Var
  {-
  | EtaExpansion
  | PartialApplication -- TODO: is a function applied to no arguments considered partially applied?
  -}
  -- Recursion etc.
  -- TODO: should we have hierarchies?
  -- TODO: is HigherArity a concept? Since there is a learning gap between
  -- single arity and higher arity functions.
  deriving (Eq, Ord, Show, Read)

type Environment = [(Var, Type Free, Set Concept)]

restrict :: Set Concept -> Environment -> Environment
restrict cs = filter \(_, _, c) -> c `Set.isSubsetOf` cs

data Technique = PointFree | EtaLong
  deriving (Eq, Ord, Show, Read)

class HasConcepts a where
  concepts :: Lens' a (MultiSet Concept)

class HasTechnique a where
  technique :: Lens' a Technique

class HasEnvironment a where
  environment :: Lens' a Environment

type WithConcepts s m = (MonadState s m, HasConcepts s)
type WithTechnique s m = (MonadState s m, HasTechnique s)
type WithEnvironment s m = (MonadState s m, HasEnvironment s)
