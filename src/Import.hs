{-# OPTIONS_GHC -fno-warn-orphans #-}

module Import
  ( module Prettyprinter
  , module RIO
  , module RIO.Text
  , module Types
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
