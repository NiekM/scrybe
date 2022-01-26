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
  { sketch :: Sketch              -- ^ The sketch
-- NOTE: The list of lists is used to not use the same option (represented by a
-- list of possible sketches) more than once
  , options :: [[(Sketch, Type)]] -- ^ Possible hole fillings
  , maxHole :: Hole               -- ^ Used to generate fresh holes
  , maxTBound :: TBound           -- ^ Used to generate fresh variables
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
step :: GenState -> [GenState]
step GenState
  { sketch = Sketch { expr, goals }
  , options
  , maxHole
  , maxTBound
  } = do
    -- Select the first goal
    ((n, goal), goals') <- toList $ Map.minViewWithKey goals
    -- Select a type along with all expressions of that type
    (sketches, options') <- pick options
    (s, t) <- sketches
    let maxTBound' = fromIntegral . length $ bound t
    -- Check that the selected unifies with the goal
    th <- toList $ unify (renumber maxTBound t) goal
    let Sketch hf new = renumber maxHole (renumber maxTBound s)
    return GenState
      { sketch = Sketch
        { expr  = subst (Map.singleton n hf) expr
        , goals = subst th (goals' <> new)
        }
      -- , options = options
      , options = options'
      , maxHole = maxHole + fromIntegral (length new)
      , maxTBound = maxTBound + maxTBound'
      }


genTree :: (state -> [state]) -> state -> Tree state
genTree next start = Node start (genTree next <$> next start)
