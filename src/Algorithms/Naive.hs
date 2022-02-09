module Algorithms.Naive where

import Import hiding (local)
import Language
import RIO.List
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import Control.Monad.State
import TermGen

-- | This is a naive type-driven synthesizer that tries to fill each hole by
-- unifying its type with all possible instantiations of expressions in the
-- environment. It introduces no language constructs (lambda abstractions, case
-- analyses etc.). However, it only uses every expression in the environment
-- once, to emulate simple synthesis parameters.
data Naive = Naive
  { expr :: Term Hole
  , goals :: Map Hole (Type Hole)
  , datatypes :: Map Ctr (Type Hole)
  , global :: Map Var (Type Hole)
  , locals :: Map Hole (Map Var (Type Hole))
  , maxHole :: Hole
  , maxFree :: Hole
  } deriving (Eq, Ord, Show)

instance Gen Naive where
  fromSketch :: Module -> Term (Type Void) -> Naive
  fromSketch Module { ctrs = datatypes, vars = global } sketch = Naive
    { expr
    , goals = fmap absurd <$> Map.fromList (holes sketch')
    , datatypes
    , global
    , locals
    , maxHole = 1 + fromMaybe 0 (Set.lookupMax $ Map.keysSet locals)
    , maxFree = 0
    } where
      sketch' = evalState (number sketch) 0
      expr = fst <$> sketch'
      locals = holeContexts Map.empty expr

  step :: Naive -> [Naive]
  step Naive
    { expr
    , goals
    , datatypes
    , global
    , locals
    , maxHole
    , maxFree
    } = do
      -- Select the first goal
      ((n, goal), goals') <- toList $ Map.minViewWithKey goals
      -- Select the corresponding environment
      local <- toList $ locals Map.!? n
      let env = Map.mapKeys Var (global <> local) <> Map.mapKeys Ctr datatypes
      -- Pick an entry from the environment
      (name, t) <- Map.toList env
      -- Renumber the type variables in ty
      let t' = (+ maxFree) <$> t
      -- Generate all ways to instantiate sketch
      (sketch, typ) <- expand name t'
      -- Check that the type unifies with the goal
      th <- toList $ unify typ goal
      -- Compute new maximum Free variable
      let maxFree' = fromMaybe 0 . maximumMaybe . free $ typ
      let sk = evalState (number sketch) maxHole
      let hf = fst <$> sk
      let new = Map.fromList . holes $ sk
      -- Copy the context to new holes
      let locals' = local <$ new
      return Naive
        { expr = subst (Map.singleton n hf) expr
        , goals = subst th <$> (goals' <> new)
        , datatypes = case name of Ctr x -> Map.delete x datatypes; _ -> datatypes
        -- Delete the used expression from the environment.
        , global = case name of Var x -> Map.delete x global; _ -> global
        -- Remove the filled hole's context
        , locals = Map.delete n locals <> locals'
        , maxHole = maxHole + fromIntegral (length new)
        , maxFree = maxFree + maxFree'
        }
