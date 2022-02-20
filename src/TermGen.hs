{-# LANGUAGE MultiParamTypeClasses #-}
module TermGen where

import Import hiding (local)
import Fresh
import Language
import Data.Tree
import Data.Coerce
import Control.Monad.RWS

-- TODO: sketch should probably not carry the environment, it should probably
-- be included in a state (or reader) transformer in GenT
data Sketch = Sketch
  { expr :: Term Hole
  , ctx :: Map Hole HoleCtx
  } deriving (Eq, Ord, Show)

newtype GenT m a = GenT (RWST Module () (Hole, Free) m a)
  deriving newtype (Functor, Applicative, Monad, Alternative)
  deriving newtype (MonadFail, MonadPlus, MonadReader Module)

instance Monad m => MonadFresh Hole (GenT m) where
  fresh = GenT . state $ fst &&& first next

instance Monad m => MonadFresh Free (GenT m) where
  fresh = GenT . state $ snd &&& second next

runGenT :: Monad m => GenT m a -> Module -> (Hole, Free) -> m (a, (Hole, Free))
runGenT (GenT m) env s = do
  (x, s', _) <- runRWST m env s
  return (x, s')

evalGenT :: Monad m => GenT m a -> Module -> (Hole, Free) -> m a
evalGenT m env s = fst <$> runGenT m env s

genTree :: forall a. (a -> GenT [] a) -> a -> GenT Tree a
genTree = coerce (search @a @Module @() @(Hole, Free))
