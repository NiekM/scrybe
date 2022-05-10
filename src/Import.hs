{-# LANGUAGE PolyKinds #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Import
  ( module RIO
  , module RIO.Text
  , module Control.Monad.RWS
  , module Lens.Micro.Platform
  , Pretty(..)
  , unsnoc
  , maximumDef
  , mfold
  , failMaybe
  , mergeMap
  , search
  , todo
  , TODO
  , distr
  ) where

import RIO hiding (local)
import RIO.Text (unpack)
import RIO.List
import qualified RIO.Map as Map
import Prettyprinter
import Data.Tree
import Control.Monad.RWS hiding (local)
import Lens.Micro.Platform

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  pretty = \case
    Left x -> pretty x
    Right y -> pretty y

instance Display (Doc ann) where
  textDisplay = fromString . show

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

mergeMap :: (Monad m, Ord k) => (v -> v -> m v) ->
  Map k v -> Map k v -> m (Map k v)
mergeMap f x y = sequence $ Map.unionWith
  (\a b -> join $ liftM2 f a b)
  (return <$> x)
  (return <$> y)

search :: forall a r w s. Monoid w =>
  (a -> RWST r w s [] a) -> a -> RWST r w s Tree a
search step init = RWST \r s -> go r (init, s, mempty) where
  go :: r -> (a, s, w) -> Tree (a, s, w)
  go r (x, s, w) = Node (x, s, w) (go r <$> runRWST (step x) r s)

{-# WARNING TODO "TODO left in code" #-}
type family TODO :: k where { }

{-# WARNING todo "Todo left in code" #-}
todo :: a
todo = error "Todo left in code"

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
