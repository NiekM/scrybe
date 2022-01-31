{-# LANGUAGE QuasiQuotes #-}
{-# options_ghc -Wno-orphans #-}

module Language.Prelude where

import Language.Syntax
import Language.Parser

prelude :: [Binding]
prelude =
  [ [bi|T :: Bool|]
  , [bi|F :: Bool|]
  , [bi|Nil :: List 0|]
  , [bi|Cons :: 0 -> List 0 -> List 0|]
  , [bi|compose :: (1 -> 2) -> (0 -> 1) -> (0 -> 2)|]
  , [bi|foldr :: (0 -> 1 -> 1) -> 1 -> List 0 -> 1|]
  ]
