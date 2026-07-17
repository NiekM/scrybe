{-# LANGUAGE MultiParamTypeClasses #-}

module Utils.Weighted ( Search, WeightedMonad(..), Dist ) where

import RIO
import Control.Monad.Search
import Control.Monad.RWS
import Data.Monoid
import Numeric.Natural

type Dist = Sum Natural

instance MonadFail (Search Dist) where
  fail _ = mzero

class Monad m => WeightedMonad m where
  weigh :: Dist -> m ()

instance WeightedMonad (Search Dist) where
  weigh = cost'

instance (Monoid w, WeightedMonad m) => WeightedMonad (RWST r w s m) where
  weigh c = RWST \_r s -> (,s,mempty) <$> weigh c
