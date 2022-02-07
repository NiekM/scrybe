{-# LANGUAGE FlexibleContexts #-}
module TermGen where

import Import hiding (local)
import Language
import Data.Tree (Tree(..), levels)
import RIO.List
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import Control.Monad.State

type Env = Map Var (Type Hole)

data GenSt = GenSt
  { expr :: Term Hole
  , goals :: Map Hole (Type Hole)
  , global :: Env
  , locals :: Map Hole Env
  , maxHole :: Hole
  , maxFree :: Hole
  } deriving (Eq, Ord, Show)

fromSketch :: Env -> Term (Type Void) -> GenSt
fromSketch global sketch = GenSt
  { expr
  , goals = fmap absurd <$> Map.fromList (holes sketch')
  , global
  , locals
  , maxHole = 1 + fromMaybe 0 (Set.lookupMax $ Map.keysSet locals)
  , maxFree = 0
  } where
    sketch' = evalState (number sketch) 0
    expr = fst <$> sketch'
    locals = holeContexts Map.empty expr

-- TODO: add weights to options, and/or add nr of uses to each option
-- TODO: can step be generalized, or should it be a method of some class over
-- genstates or environments?
step :: GenSt -> [GenSt]
step GenSt
  { expr
  , goals
  , global
  , locals
  , maxHole
  , maxFree
  } = do
    -- Select the first goal
    ((n, goal), goals') <- toList $ Map.minViewWithKey goals
    -- Select the corresponding environment
    local <- toList $ locals Map.!? n
    -- Pick an entry from the environment
    (name, t) <- Map.toList (global <> local)
    -- Renumber the type variables in ty
    let t' = (+ maxFree) <$> t
    -- Generate all ways to instantiate sketch
    (sketch, typ) <- expand (Var name) t'
    -- Check that the type unifies with the goal
    th <- toList $ unify typ goal
    -- Compute new maximum Free variable
    let maxFree' = fromMaybe 0 . maximumMaybe . free $ typ
    let sk = evalState (number sketch) maxHole
    let hf = fst <$> sk
    let new = Map.fromList . holes $ sk
    -- Copy the context to new holes
    let locals' = local <$ new
    return GenSt
      { expr = subst (Map.singleton n hf) expr
      , goals = subst th <$> (goals' <> new)
      , global = Map.delete name global
      , locals = Map.delete n locals <> locals'
      , maxHole = maxHole + fromIntegral (length new)
      , maxFree = maxFree + maxFree'
      }

genTree :: (state -> [state]) -> state -> Tree state
genTree next start = Node start (genTree next <$> next start)

synthesize :: Env -> Term (Type Void) -> [[GenSt]]
synthesize env = levels . genTree step . fromSketch env
