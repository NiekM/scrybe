{-# LANGUAGE MultiParamTypeClasses #-}
module Fresh where

import Import

class Next a where
  next :: a -> a

instance Next Int where
  next = (+ 1)

class Monad m => MonadFresh a m where
  fresh :: m a
