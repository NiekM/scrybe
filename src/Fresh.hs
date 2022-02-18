{-# LANGUAGE MultiParamTypeClasses #-}
module Fresh where

import Import
import Control.Monad.State

class Next a where
  next :: a -> a

instance Next Int where
  next = (+ 1)

newtype FreshT s m a = FreshT (StateT s m a)
  deriving newtype (Functor, Applicative, Monad, Alternative)
  deriving newtype (MonadFail, MonadPlus, MonadState s, MonadTrans)

runFreshT :: FreshT s m a -> s -> m (a, s)
runFreshT (FreshT m) = runStateT m

evalFreshT :: Monad m => FreshT s m a -> s -> m a
evalFreshT m s = do
  ~(x, _) <- runFreshT m s
  return x

mapFreshT :: (m (a, s) -> n (b, s)) -> FreshT s m a -> FreshT s n b
mapFreshT f m = FreshT . StateT $ f . runFreshT m

type Fresh s = FreshT s Identity

runFresh :: Fresh s a -> s -> (a, s)
runFresh m = runIdentity . runFreshT m

evalFresh :: Fresh s a -> s -> a
evalFresh m = fst . runFresh m

class Monad m => MonadFresh a m where
  fresh :: m a

instance (Monad m, Next s) => MonadFresh s (FreshT s m) where
  fresh = FreshT . state $ id &&& next

instance (Monad m, Next s) => MonadFresh s (FreshT (s, t) m) where
  fresh = FreshT . state $ fst &&& first next

instance (Monad m, Next t) => MonadFresh t (FreshT (s, t) m) where
  fresh = FreshT . state $ snd &&& second next
