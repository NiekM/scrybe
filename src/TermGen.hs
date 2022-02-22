{-# LANGUAGE MultiParamTypeClasses #-}
module TermGen where

import Import
import Language
import Data.Tree
import Data.Coerce
import Control.Monad.RWS

data GenState = GenState
  { _holeCtxs :: Map Hole HoleCtx
  , _module   :: Module
  , freshHole :: Hole
  , freshFree :: Free
  , freshVar  :: Int
  }

instance HasHoleCtxs GenState where
  holeCtxs = lens _holeCtxs \x y -> x { _holeCtxs = y }

instance HasModule GenState where
  env = lens _module \x y -> x { _module = y }

mkGenState :: Module -> GenState
mkGenState e = GenState mempty e 0 0 0

-- TODO: The Module should probably be included in the GenState, since it might
-- be updated, such as reducing occurences.
-- TODO: Add a readonly Options datatype that can be used to choose between
-- different kinds of synthesis.
newtype GenT m a = GenT (RWST () () GenState m a)
  deriving newtype (Functor, Applicative, Monad, Alternative)
  deriving newtype (MonadFail, MonadPlus)
  deriving newtype (MonadState GenState)

instance Monad m => MonadFresh Hole (GenT m) where
  fresh = GenT . state $ \g@GenState { freshHole } ->
    (freshHole, g { freshHole = 1 + freshHole })

instance Monad m => MonadFresh Free (GenT m) where
  fresh = GenT . state $ \g@GenState { freshFree } ->
    (freshFree, g { freshFree = 1 + freshFree })

instance Monad m => MonadFresh Var (GenT m) where
  fresh = GenT . state $ \g@GenState { freshVar } ->
    (nVar freshVar, g { freshVar = 1 + freshVar })

runGenT :: Monad m => GenT m a -> GenState -> m (a, GenState)
runGenT (GenT m) s = do
  (x, s', _) <- runRWST m () s
  return (x, s')

evalGenT :: Monad m => GenT m a -> GenState -> m a
evalGenT m s = fst <$> runGenT m s

genTree :: forall a. (a -> GenT [] a) -> a -> GenT Tree a
genTree = coerce (search @a @() @() @GenState)
