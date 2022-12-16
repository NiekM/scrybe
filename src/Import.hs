{-# OPTIONS_GHC -fno-warn-orphans #-}

module Import
  ( module RIO
  , module Control.Monad.RWS
  , module Lens.Micro.Platform
  , module Data.Functor.Compose
  , module Utils.BoundedLattice
  , module Utils.Fresh
  , module Utils.Map
  , module Utils.PartialSemigroup
  , module Utils.Type
  , Pretty(..), Doc, (<+>)
  , Dist
  , Unit(Unit)
  , Possibly(..), possibly
  , unsnoc
  , mfold
  , failMaybe
  ) where

import RIO hiding (local, assert, and, or)
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import Prettyprinter
import Control.Monad.RWS hiding (local, All)
import Lens.Micro.Platform
import Data.Functor.Compose
import Data.Monus.Dist (Dist)
import Utils.BoundedLattice
import Utils.Fresh
import Utils.Map
import Utils.PartialSemigroup
import Utils.Type

newtype Unit = MkUnit ()
  deriving newtype (Eq, Ord, Show, Read, Semigroup, Monoid, NFData)

pattern Unit :: Unit
pattern Unit = MkUnit ()

instance Pretty Unit where
  pretty _ = space

data Possibly a = Yes | No | Perhaps a
  deriving (Eq, Ord, Show)

possibly :: [Possibly a] -> Maybe [a]
possibly = sequence . mapMaybe \case
  No -> Nothing
  Yes -> Just Nothing
  Perhaps x -> Just $ Just x

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  pretty = \case
    Left x -> pretty x
    Right y -> pretty y

instance (Pretty a, Pretty b) => Pretty (Map a b) where
  pretty m = align . Prettyprinter.list $ Map.assocs m <&> \(k, x) ->
    pretty k <> ":" <+> align (pretty x)

instance Pretty (f (g a)) => Pretty (Compose f g a) where
  pretty (Compose x) = pretty x

instance Pretty a => Pretty (Set a) where
  pretty = pretty . Set.toList

instance Display (Doc ann) where
  textDisplay = fromString . show

instance Pretty a => Pretty (Logic a) where
  pretty = \case
    Pure a -> pretty a
    Conjunction xs -> "/\\" <+> align (pretty xs)
    Disjunction xs -> "\\/" <+> align (pretty xs)

unsnoc :: NonEmpty a -> ([a], a)
unsnoc (x :| []) = ([], x)
unsnoc (x :| y:ys) = (x:) `first` unsnoc (y :| ys)

mfold :: (Foldable f, MonadPlus m) => f a -> m a
mfold = msum . fmap return . toList

failMaybe :: MonadFail m => Maybe a -> m a
failMaybe = \case
  Nothing -> fail ""
  Just x -> return x
