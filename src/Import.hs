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
  , Logic(..)
  , BoundedLattice(..)
  , unsnoc
  , maximumDef
  , mfold
  , failMaybe
  , liftRWST
  , evalState
  , readerState
  , readerStateMaybe
  , readerStateT
  , distr
  ) where

import RIO hiding (local, assert, and, or)
import RIO.List
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import Prettyprinter
import Control.Monad.RWS hiding (local, All)
import Control.Monad.Reader
import Control.Monad.State
import Lens.Micro.Platform
import Data.Functor.Compose
import Data.Monus.Dist (Dist)
import Utils.BoundedLattice
import Utils.Fresh
import Utils.Map
import Utils.PartialSemigroup
import Utils.Type

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

maximumDef :: (Ord a, Foldable f) => a -> f a -> a
maximumDef d = fromMaybe d . maximumMaybe

mfold :: (Foldable f, MonadPlus m) => f a -> m a
mfold = msum . fmap return . toList

failMaybe :: MonadFail m => Maybe a -> m a
failMaybe = \case
  Nothing -> fail ""
  Just x -> return x

liftRWST :: (Monad m, Monoid w) => m a -> RWST r w s m a
liftRWST m = RWST \_r s -> (,s,mempty) <$> m

readerState :: State s (Reader s a) -> Reader s a
readerState st = do
  (x, s) <- asks $ runState st
  local (const s) x

readerStateT :: Monad m => StateT s m (ReaderT s m a) -> ReaderT s m a
readerStateT st = do
  x <- asks $ runStateT st
  (y, s) <- ReaderT $ const x
  local (const s) y

readerStateMaybe :: StateT s Maybe (Reader s a) -> Reader s (Maybe a)
readerStateMaybe st = do
  x <- asks $ runStateT st
  case x of
    Nothing -> return Nothing
    Just (y, s) -> local (const s) (return <$> y)

distr :: MonadPlus m => Int -> Int -> m [Int]
distr _ k | k <= 0 = mzero
distr n 1 = return [n]
distr n k = case compare n k of
  LT -> mzero
  EQ -> return $ replicate k 1
  GT -> do
    x <- mfold [1 .. n - k + 1]
    xs <- distr (n - x) (k - 1)
    return $ x : xs
