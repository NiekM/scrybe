{-# LANGUAGE FlexibleContexts #-}
module TermGen where

import Import
import Language
import Data.Generics.Uniplate.Data (transformBi)
import Data.Tree (Tree(..), levels)
import RIO.List
import qualified RIO.Map as Map
import qualified RIO.Set as Set

data GenState = GenState
  { sketch :: Sketch Hole
  , env :: Env Hole
  , contexts :: Map Hole (Env Hole)
  , maxHole :: Hole
  , maxFree :: Hole
  } deriving (Eq, Read, Show)

fromSketch :: Env Hole -> Sketch Hole -> GenState
fromSketch env sketch@Sketch{ expr } = GenState
  { sketch
  , env
  , contexts
  , maxHole = 1 + fromMaybe 0 (Set.lookupMax $ Map.keysSet contexts)
  , maxFree = 0
  } where contexts = holeContexts Map.empty expr

-- | All possible ways to take one value from a list.
pick :: [a] -> [(a, [a])]
pick [] = []
pick (x:xs) = (x,xs) : (second (x:) <$> pick xs)

-- | Renumbers all numbers of a specific type in a datatype by increasing them
-- by a fixed amount.
renumber :: (Num k, Data k, Data a) => k -> a -> a
renumber = transformBi . (+)

-- TODO: add weights to options, and/or add nr of uses to each option
step :: GenState -> [GenState]
step GenState
  { sketch = Sketch { expr, goals }
  , env
  , contexts
  , maxHole
  , maxFree
  } = do
    -- Select the first goal
    ((n, goal), goals') <- toList $ Map.minViewWithKey goals
    -- Select the corresponding environment
    ctx <- toList $ contexts Map.!? n
    -- Pick an entry from the environment
    (name, ty) <- Map.toList (env <> ctx)
    -- Wrap name into a sketch
    let sk = Sketch (EVar name) mempty
    -- Renumber the type variables in ty
    let ty' = renumber maxFree ty
    -- Generate all ways to instantiate sketch
    (sketch, typ) <- expand maxHole sk ty'
    -- Check that the type unifies with the goal
    th <- toList $ unify typ goal
    -- Compute new maximum Free variable
    let maxFree' = fromMaybe 0 . maximumMaybe . free $ typ
    -- Renumber type variables of sketch
    let Sketch hf new = sketch
    -- Copy the context to new holes
    let contexts' = ctx <$ new
    return GenState
      { sketch = Sketch
        { expr = subst (Map.singleton n hf) expr
        , goals = subst th <$> (goals' <> new)
        }
      , env = Map.delete name env
      , contexts = Map.delete n contexts <> contexts'
      , maxHole = maxHole + fromIntegral (length new)
      , maxFree = maxFree + maxFree'
      }

genTree :: (state -> [state]) -> state -> Tree state
genTree next start = Node start (genTree next <$> next start)

synthesize :: Env Hole -> Sketch Hole -> [[GenState]]
synthesize env = levels . genTree step . fromSketch env
