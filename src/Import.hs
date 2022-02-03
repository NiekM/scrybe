{-# OPTIONS_GHC -fno-warn-orphans #-}

module Import
  ( module Prettyprinter
  , module RIO
  , module RIO.Text
  , module Types
  , prettyParens
  , subst
  , compose
  ) where

import RIO
import RIO.Text (unpack)
import qualified RIO.Map as Map
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

subst :: (Monad m, Ord a) => Map a (m a) -> m a -> m a
subst th e = e >>= \i -> fromMaybe (return i) (Map.lookup i th)

compose :: (Monad m, Ord a) => Map a (m a) -> Map a (m a) -> Map a (m a)
compose sigma gamma = Map.unions
  [ subst sigma <$> gamma
  , Map.withoutKeys sigma (Map.keysSet gamma)
  ]
