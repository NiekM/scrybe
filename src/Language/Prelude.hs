module Language.Prelude where

import Import
import Language.Syntax
import Language.Parser
import Text.Megaparsec

prelude :: [(Var, Type Hole)]
prelude = fromRight undefined
  . parse ((,) <$> parser <* symbol "::" <*> parser) "" <$>
  [ "T :: Bool"
  , "F :: Bool"
  , "Nil :: List {0}"
  , "Cons :: {0} -> List {0} -> List {0}"
  , "compose :: ({1} -> {2}) -> ({0} -> {1}) -> ({0} -> {2})"
  , "foldr :: ({0} -> {1} -> {1}) -> {1} -> List {0} -> {1}"
  ]
