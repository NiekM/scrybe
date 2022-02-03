{-# LANGUAGE FlexibleContexts #-}
module TermGen where

import Import
import Language
import Data.Tree (Tree(..), levels)
import RIO.List
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import Control.Monad.State

data GenSt = GenSt
  { expr :: Expr Hole
  , goals :: Map Hole (Type Hole)
  , env :: Env Hole
  , ctxs :: Map Hole (Env Hole)
  , maxHole :: Hole
  , maxFree :: Hole
  } deriving (Eq, Read, Show)

fromSketch :: Env Hole -> Expr (Type Void) -> GenSt
fromSketch env sketch = GenSt
  { expr
  , goals = fmap absurd <$> Map.fromList (holes sketch')
  , env
  , ctxs
  , maxHole = 1 + fromMaybe 0 (Set.lookupMax $ Map.keysSet ctxs)
  , maxFree = 0
  } where
    sketch' = evalState (number sketch) 0
    expr = fst <$> sketch'
    ctxs = holeContexts Map.empty expr

-- TODO: add weights to options, and/or add nr of uses to each option
step :: GenSt -> [GenSt]
step GenSt
  { expr
  , goals
  , env
  , ctxs
  , maxHole
  , maxFree
  } = do
    -- Select the first goal
    ((n, goal), goals') <- toList $ Map.minViewWithKey goals
    -- Select the corresponding environment
    ctx <- toList $ ctxs Map.!? n
    -- Pick an entry from the environment
    (name, t) <- Map.toList (env <> ctx)
    -- Renumber the type variables in ty
    -- let t' = fst <$> evalState (number t) maxFr
    let t' = (+ maxFree) <$> t
    -- Generate all ways to instantiate sketch
    (sketch, typ) <- xpnd (EVar name) t'
    -- Check that the type unifies with the goal
    th <- toList $ unify typ goal
    -- Compute new maximum Free variable
    let maxFree' = fromMaybe 0 . maximumMaybe . free $ typ
    let sk = evalState (number sketch) maxHole
    let hf = fst <$> sk
    let new = Map.fromList . holes $ sk
    -- Copy the context to new holes
    let ctxts' = ctx <$ new
    return GenSt
      { expr = subst (Map.singleton n hf) expr
      , goals = subst th <$> (goals' <> new)
      , env = Map.delete name env
      , ctxs = Map.delete n ctxs <> ctxts'
      , maxHole = maxHole + fromIntegral (length new)
      , maxFree = maxFree + maxFree'
      }

genTree :: (state -> [state]) -> state -> Tree state
genTree next start = Node start (genTree next <$> next start)

synthesize :: Env Hole -> Expr (Type Void) -> [[GenSt]]
synthesize env = levels . genTree step . fromSketch env
