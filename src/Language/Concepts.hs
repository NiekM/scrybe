{-# LANGUAGE RankNTypes #-}
module Language.Concepts where

import Import
import Language.Syntax
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

fromList :: Ord a => [a] -> MultiSet a
fromList = fmap Just . Map.fromListWith (+) . fmap (,1)

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

-- | Concepts within our language, used to categorize hole fillings.
data Concept
  = Func Var
  | CCtr Ctr
  {-
  | EtaExpansion
  | PartialApplication -- TODO: is a function applied to no arguments considered partially applied?
  -}
  -- Recursion etc.
  -- TODO: should we have hierarchies?
  -- TODO: is HigherArity a concept? Since there is a learning gap between
  -- single arity and higher arity functions.
  deriving (Eq, Ord, Show, Read)

type Environment = Map Var (Poly, Set Concept)

-- TODO: use polytypes and skolemnization
fromModule :: Module Void -> Environment
fromModule m = flip Map.mapWithKey (functions m)
  \x (_, t) -> (t, Set.singleton $ Func x)

restrict :: Set Concept -> Environment -> Environment
restrict cs = Map.filter \(_, c) -> c `Set.isSubsetOf` cs

-- TODO: take into account Poly variables
-- TODO: actually gather concepts from variables/constructors/language
-- constructs
fromSketch :: Module Void -> Ann Type 'Term Var a ->
  (Environment, MultiSet Concept)
fromSketch m e = (Map.fromList xs, fromList $ Func . view _1 <$> xs) where
  fs = Map.keys . functions $ m
  xs = collect e >>= \(Annot x u) -> case x of
    Var v | v `elem` fs -> [(v, (Poly [] u, Set.singleton $ Func v))]
    _ -> []

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
