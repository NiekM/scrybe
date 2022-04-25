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
  , search
  , todo
  , TODO
  ) where

import RIO hiding (local)
import RIO.Text (unpack)
import RIO.List
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

search :: forall a r w s. Monoid w =>
  (a -> RWST r w s [] a) -> a -> RWST r w s Tree a
search step init = RWST \r s -> go r (init, s, mempty) where
  go :: r -> (a, s, w) -> Tree (a, s, w)
  go r (x, s, w) = Node (x, s, w) (go r <$> runRWST (step x) r s)

{-# WARNING TODO "TODO left in code" #-}
data TODO

{-# WARNING todo "Todo left in code" #-}
todo :: a
todo = error "Todo left in code"
