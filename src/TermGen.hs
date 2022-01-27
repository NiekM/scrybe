{-# LANGUAGE FlexibleContexts #-}
module TermGen where

import Import
import Lang
import Unify
import Subst
import Data.Generics.Uniplate.Data (transformBi)
import Data.Tree (Tree(..))
import qualified RIO.Map as Map

data GenState = GenState
  { sketch :: Sketch
  , contexts :: Map Hole Env
  , maxHole :: Hole
  , maxFree :: TFree
  } deriving (Eq, Read, Show)

-- | All possible ways to take one value from a list.
pick :: [a] -> [(a, [a])]
pick [] = []
pick (x:xs) = (x,xs) : (second (x:) <$> pick xs)

-- | Renumbers all numbers of a specific type in a datatype by increasing them
-- by a fixed amount.
renumber :: (Num k, Data k, Data a) => k -> a -> a
renumber = transformBi . (+)

-- TODO: add weights to options, and/or add nr of uses to each option
-- TODO: there should probably be a shared global environment as well as
-- individual, local environments for each hole. The local environments should
-- be small enough that it does not really make a difference.
step :: GenState -> [GenState]
step GenState
  { sketch = Sketch { expr, goals }
  , contexts
  , maxHole
  , maxFree
  } = do
    -- Select the first goal
    ((n, goal), goals') <- toList $ Map.minViewWithKey goals
    -- Select the corresponding environment
    -- TODO: actually use separate environments
    env <- toList $ contexts Map.!? 0
    -- Pick an entry from the environment
    (name, ty) <- Map.toList env
    -- Wrap name into a sketch
    let sk = Sketch (EVar (Left (Bound name))) mempty
    -- Renumber the type variables in ty
    let ty' = renumber maxFree ty
    -- Generate all ways to instantiate sketch
    (sketch, typ) <- expand maxHole sk ty'
    -- Check that the type unifies with the goal
    th <- toList $ unify typ goal
    -- Compute new maximum Free variable
    let maxFree' = fromIntegral . length . free $ typ
    -- Renumber type variables of sketch
    let Sketch hf new = sketch
    return GenState
      { sketch = Sketch
        { expr = subst (Map.singleton n hf) expr
        , goals = subst th (goals' <> new)
        }
      , contexts = Map.adjust (Map.delete name) 0 contexts
      , maxHole = maxHole + fromIntegral (length new)
      , maxFree = maxFree + maxFree'
      }

genTree :: (state -> [state]) -> state -> Tree state
genTree next start = Node start (genTree next <$> next start)
