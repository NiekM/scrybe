{-# LANGUAGE QuasiQuotes #-}
{-# options_ghc -Wno-orphans #-}

module Language.Prelude where

import Language.Syntax
import Language.Parser

prelude :: [Binding]
prelude =
  [ [bi|T :: Bool|]
  , [bi|F :: Bool|]
  , [bi|Nil :: List _0|]
  , [bi|Cons :: _0 -> List _0 -> List _0|]
  , [bi|compose :: (_1 -> _2) -> (_0 -> _1) -> (_0 -> _2)|]
  , [bi|foldr :: (_0 -> _1 -> _1) -> _1 -> List _0 -> _1|]
  ]
