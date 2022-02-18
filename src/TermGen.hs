{-# LANGUAGE FlexibleContexts #-}
module TermGen where

import Import hiding (local)
import Fresh
import Language
import Data.Tree
import Data.Coerce

data Sketch = Sketch
  { expr :: Term Hole
  , env :: Module
  , ctx :: Map Hole HoleCtx
  } deriving (Eq, Ord, Show)

newtype GenT m a = GenT (FreshT (Hole, Free) m a)
  deriving newtype (Functor, Applicative, Monad, Alternative, MonadTrans)
  deriving newtype (MonadFail, MonadPlus, MonadFresh Hole, MonadFresh Free)

runGenT :: Hole -> Free -> GenT m a -> m (a, (Hole, Free))
runGenT h f (GenT m) = runFreshT m (h, f)

evalGenT :: Monad m => Hole -> Free -> GenT m a -> m a
evalGenT h f (GenT m) = evalFreshT m (h, f)

mapGenT :: (m (a, (Hole, Free)) -> n (b, (Hole, Free))) -> GenT m a -> GenT n b
mapGenT f (GenT m) = GenT $ mapFreshT f m

type Gen = GenT Identity

runGen :: Hole -> Free -> Gen a -> (a, (Hole, Free))
runGen h f = runIdentity . runGenT h f

evalGen :: Hole -> Free -> Gen a -> a
evalGen h f = fst . runGen h f

genTree :: forall a. (a -> GenT [] a) -> a -> GenT Tree a
genTree = coerce (search @a @(Hole, Free))
