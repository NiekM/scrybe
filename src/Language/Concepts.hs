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

type SynMonad s m =
  ( WithEnvironment s m, WithConcepts s m
  , WithTechnique s m, WithHoleCtxs s m
  , FreshVar m, FreshHole m, FreshFree m
  , MonadFail m
  )

processSketch :: (MonadState s m, HasTechnique s, HasHoleCtxs s, FreshVar m)
  => Term Hole -> m (Term Hole)
processSketch e = use technique >>= \case
  EtaLong -> etaExpand e
  _ -> return e

type HoleFilling = (Term (Type Free), Type Free)

-- NOTE: this function does not take into account any newly introduced
-- variables in the sketch, this should be done separately with a call to
-- etaExpand
tryHoleFilling :: SynMonad s m => HoleCtx -> HoleFilling -> m (Term Hole)
tryHoleFilling HoleCtx { goal, local } (e, t) = do
  -- TODO: refresh type variables of holefilling?
  th <- unify t goal
  x <- forM e \u -> do
    h <- fresh
    modifying holeCtxs $ Map.insert h HoleCtx { goal = u, local }
    return h
  modifying holeCtxs . fmap $ substCtx th
  return x

-- TODO: should this function get a holeCtx as input?

fullyApply :: (Var, Type Free) -> HoleFilling
fullyApply (name, t) = first (apps . (Var name :|) . fmap Hole) (splitArgs t)

pick :: (SynMonad s m, MonadPlus m) => HoleCtx -> m (Term Hole)
pick ctx = do
  -- Compute hole fillings from local variables.
  let locals = fmap ((,Set.empty) . fullyApply) . Map.assocs $ local ctx
  -- Compute hole fillings from global variables.
  let globals = use environment >>= \m -> do
      (name, t, c) <- mfold m
      u <- renumber t
      return (fullyApply (name, u), c)
  -- Choose hole fillings from either local or global variables.
  (hf, cs) <- mfold locals <|> globals
  -- Check if the hole fillings fit.
  e <- tryHoleFilling ctx hf
  -- Remove the used concepts.
  modifying concepts (`sub` fromSet cs)
  -- Remove functions from the environment that use removed concepts
  cs' <- use concepts
  modifying environment . restrict $ Map.keysSet cs'
  processSketch e

-- TODO: expand does not return 'all' ways to add holes to an
-- expression, since its return type might unify with a function type.
expand :: Term (Type Free) -> Type Free -> [(Term (Type Free), Type Free)]
expand e t = (e, t) : case t of
  Arr t1 t2 -> expand (App e (Hole t1)) t2
  _ -> []
