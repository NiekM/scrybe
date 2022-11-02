{-# LANGUAGE MultiParamTypeClasses #-}

module Utils.Weighted where

import RIO
import Control.Monad.Heap
import Control.Monad.RWS
import Data.Monus

newtype Search c a = Search { runSearch :: Heap c a }
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (Alternative, MonadPlus)

instance MonadFail (Search c) where
  fail _ = mzero

class Monad m => WeightedMonad c m where
  weigh :: c -> m ()

instance Monus c => WeightedMonad c (Search c) where
  weigh c = Search $ tell c

instance (Monoid w, WeightedMonad c m) => WeightedMonad c (RWST r w s m) where
  weigh c = RWST \_r s -> (,s,mempty) <$> weigh c
