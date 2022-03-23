{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- NOTE: this module is purely for debugging purposes and to load some
-- functions into ghci
module Debug where

import Import
import Language
import Text.Megaparsec
import RIO.Text

instance Parse a => IsString a where
 fromString t = fromMaybe (error "Parse failed") $
   parseMaybe lex (pack t) >>= parseMaybe parser

-- TODO: load in prelude and initial GenState for experimentation
