module Language.Prelude where

import Import
import Language.Syntax
import Language.Parser

-- TODO: load preludes/modules and such from files.
prelude :: [Binding (Type Hole)]
prelude = parseUnsafe <$>
  [ "T :: Bool"
  , "F :: Bool"
  , "Nil :: List {0}"
  , "Cons :: {0} -> List {0} -> List {0}"
  , "compose :: ({1} -> {2}) -> ({0} -> {1}) -> ({0} -> {2})"
  , "foldr :: ({0} -> {1} -> {1}) -> {1} -> List {0} -> {1}"
  ]
