{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Fresh where

import Import
import Control.Monad.State

class Next a where
  next :: a -> a

instance Next Int where
  next = (+ 1)

newtype FreshT s m a = FreshT (StateT s m a)
  deriving newtype (Functor, Applicative, Monad, MonadState s, MonadTrans)

runFreshT :: FreshT s m a -> s -> m (a, s)
runFreshT (FreshT m) = runStateT m

evalFreshT :: Monad m => FreshT s m a -> s -> m a
evalFreshT m s = do
  ~(x, _) <- runFreshT m s
  return x

type Fresh s = FreshT s Identity

runFresh :: Fresh s a -> s -> (a, s)
runFresh m = runIdentity . runFreshT m

evalFresh :: Fresh s a -> s -> a
evalFresh m = fst . runFresh m

class Monad m => MonadFresh a m where
  fresh :: m a

instance (Monad m, Next s) => MonadFresh s (FreshT s m) where
  fresh = FreshT $ state (id &&& next)
