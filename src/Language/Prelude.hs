module Language.Prelude where

import Import
import Language.Syntax
import Language.Parser
import qualified RIO.Map as Map

-- TODO: load preludes/modules and such from files.
prelude :: Module
prelude = Module
  { ctrs = Map.fromList
    $ parseUnsafe ((,) <$> parser <* symbol "::" <*> parser) <$>
    [ "True :: Bool"
    , "False :: Bool"
    , "Nil :: List {0}"
    , "Cons :: {0} -> List {0} -> List {0}"
    ]
  , vars = Map.fromList
    $ parseUnsafe ((,) <$> parser <* symbol "::" <*> parser) <$>
    [ "compose :: ({1} -> {2}) -> ({0} -> {1}) -> ({0} -> {2})"
    , "foldr :: ({0} -> {1} -> {1}) -> {1} -> List {0} -> {1}"
    ]
  }
