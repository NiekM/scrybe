{-# OPTIONS_GHC -fno-warn-orphans #-}

module Import
  ( module RIO
  , module RIO.Text
  , Pretty(..)
  , subst
  , compose
  , unsnoc
  , maximumDef
  ) where

import RIO
import RIO.Text (unpack)
import RIO.List
import qualified RIO.Map as Map
import Prettyprinter

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  pretty = \case
    Left x -> pretty x
    Right y -> pretty y

instance Display (Doc ann) where
  textDisplay = fromString . show

subst :: (Monad m, Ord a) => Map a (m a) -> m a -> m a
subst th e = e >>= \i -> fromMaybe (return i) (Map.lookup i th)

compose :: (Monad m, Ord a) => Map a (m a) -> Map a (m a) -> Map a (m a)
compose sigma gamma = Map.unions
  [ subst sigma <$> gamma
  , Map.withoutKeys sigma (Map.keysSet gamma)
  ]

unsnoc :: NonEmpty a -> ([a], a)
unsnoc (x :| []) = ([], x)
unsnoc (x :| y:ys) = (x:) `first` unsnoc (y :| ys)

maximumDef :: (Ord a, Foldable f) => a -> f a -> a
maximumDef d = fromMaybe d . maximumMaybe
