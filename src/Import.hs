{-# OPTIONS_GHC -fno-warn-orphans #-}

module Import
  ( module RIO
  , module RIO.Text
  , module RIO.PrettyPrint
  , module Types
  ) where

import RIO
import RIO.Text (unpack)
import RIO.PrettyPrint
import Types

instance Pretty Text where
  pretty = fromString . unpack

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  pretty = \case
    Left x -> pretty x
    Right y -> pretty y
