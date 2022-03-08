module Language.Prelude where

import Import
import Language.Syntax
import Language.Parser
import qualified RIO.Map as Map

-- TODO: select a set of functions to form a dataset/benchmark of functions
-- that we should be able to synthesize, consisting of simple prelude functions
-- such as map, foldr, compose, const, etc. as well as slightly more
-- complicated functions from e.g. Ask-Elle, Haskell99, Myth. Implement model
-- solutions for these functions and generate input output examples to be used
-- for program synthesis.

-- TODO: curate a set of techniques and language constructs used in this set of
-- functions that we would like to be able to learn and would like to be able
-- to extract from model solutions and use to prune the synthesis.

-- TODO: load preludes/modules and such from files.
-- TODO: use poly types
mapPrelude :: Module
mapPrelude = Module
  { ctrs = Map.fromList
    $ parseUnsafe ((,) <$> parser <* symbol "::" <*> parser) <$>
    [ "True :: forall . Bool"
    , "False :: forall . Bool"
    , "Nil :: forall 0. List {0}"
    , "Cons :: forall 0. {0} -> List {0} -> List {0}"
    ]
  , vars = Map.fromList
    $ parseUnsafe ((,) <$> parser <* symbol "::" <*> parser) <$>
    [ "foldr :: forall 0 1. ({0} -> {1} -> {1}) -> {1} -> List {0} -> {1}"
    , "compose :: forall 0 1 2. ({1} -> {2}) -> ({0} -> {1}) -> ({0} -> {2})"
    ]
  }
