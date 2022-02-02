{-# OPTIONS_GHC -fno-warn-orphans #-}

module Import
  ( module Prettyprinter
  , module RIO
  , module RIO.Text
  , module Types
  , prettyParens
  ) where

import RIO
import RIO.Text (unpack)
import Prettyprinter
import Types

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  pretty = \case
    Left x -> pretty x
    Right y -> pretty y

instance Display (Doc ann) where
  textDisplay = fromString . show

prettyParens :: Pretty a => a -> (a -> Bool) -> Doc ann
prettyParens t p
  | p t = parens (pretty t)
  | otherwise = pretty t
