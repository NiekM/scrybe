{-# LANGUAGE PolyKinds #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Import
  ( module RIO
  , module Control.Monad.RWS
  , module Lens.Micro.Platform
  , module Data.Functor.Compose
  , Pretty(..)
  , Logic(..)
  , BoundedLattice(..)
  , unsnoc
  , maximumDef
  , mfold
  , failMaybe
  , readerState
  , forMap
  , unionWithM, unionsWithM, fromListM
  , distr
  ) where

import RIO hiding (local, assert, and, or)
import RIO.List
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import Prettyprinter
import Control.Monad.RWS hiding (local)
import Control.Monad.Reader
import Control.Monad.State
import Lens.Micro.Platform
import Data.Functor.Compose

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

data Logic a
  = Pure a
  | Conjunction [Logic a]
  | Disjunction [Logic a]
  deriving (Eq, Ord, Show, Read)
  deriving (Functor, Foldable, Traversable)

-- TODO: check that these instances make sense
instance Applicative Logic where
  pure = Pure
  Pure f <*> x = f <$> x
  Conjunction fs <*> x = Conjunction $ fs <&> (<*> x)
  Disjunction fs <*> x = Disjunction $ fs <&> (<*> x)

instance Monad Logic where
  Pure x >>= f = f x
  Conjunction xs >>= f = Conjunction $ xs <&> (>>= f)
  Disjunction xs >>= f = Disjunction $ xs <&> (>>= f)

instance Pretty a => Pretty (Logic a) where
  pretty = \case
    Pure a -> pretty a
    Conjunction xs -> "/\\" <+> align (pretty xs)
    Disjunction xs -> "\\/" <+> align (pretty xs)

-- TODO: this is a bounded lattice along with a way to capture a
-- (Scope, Hole, Ex) triple
class BoundedLattice a where
  top, bot :: a
  top = conj []
  bot = disj []
  and, or :: a -> a -> a
  and x y = conj [x,y]
  or x y = disj [x,y]
  conj, disj :: [a] -> a

instance BoundedLattice (Logic a) where
  conj = Conjunction
  disj = Disjunction

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

readerState :: State s (Reader s a) -> Reader s a
readerState st = do
  (x, s) <- asks $ runState st
  local (const s) x

forMap :: (Ord k, Monad m) => Map k v -> (k -> v -> m a) -> m (Map k a)
forMap m f = Map.fromList <$> for (Map.assocs m) \(k, v) -> (k,) <$> f k v

-- TODO: rename these unionWithM, unionsWithM and fromListM
unionWithM :: (Monad m, Ord k) => (v -> v -> m v) ->
  Map k v -> Map k v -> m (Map k v)
unionWithM f x y = sequence $ Map.unionWith
  (\a b -> join $ liftM2 f a b)
  (return <$> x)
  (return <$> y)

unionsWithM :: (Monad m, Foldable f, Ord k) => (v -> v -> m v) ->
  f (Map k v) -> m (Map k v)
unionsWithM f = foldl' (\x y -> x >>= unionWithM f y) $ return mempty

fromListM :: (Monad m, Foldable f, Functor f, Ord k) =>
  (v -> v -> m v) -> f (k, v) -> m (Map k v)
fromListM f = unionsWithM f . fmap (uncurry Map.singleton)

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
