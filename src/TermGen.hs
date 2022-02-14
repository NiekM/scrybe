{-# LANGUAGE FlexibleContexts #-}
module TermGen where

import Import hiding (local)
import Language
import Data.Tree (Tree(..), levels)

-- TODO: perhaps an associated data type of options/parameters is appropriate?
-- For example the naive synthesizer could have an option of whether to use
-- functions more than once.
class Gen a where
  fromSketch :: Module -> Term (Type Hole) -> a
  result :: a -> Term Hole
  step :: a -> [a]

genTree :: (state -> [state]) -> state -> Tree state
genTree next start = Node start (genTree next <$> next start)

synthesize :: Gen a => Module -> Term (Type Hole) -> [[a]]
synthesize env = levels . genTree step . fromSketch env

-- TODO: implement 'naive' program synthesis with eta expansion
