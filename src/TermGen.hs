{-# LANGUAGE MultiParamTypeClasses #-}
module TermGen where

import Import hiding (local)
import Fresh
import Language
import Data.Tree
import Data.Coerce
import Control.Monad.RWS

data GenState = GenState
  { ctxs :: Map Hole HoleCtx
  , freshHole :: Hole
  , freshFree :: Free
  , freshInt :: Int
  }

emptyGenState :: GenState
emptyGenState = GenState mempty 0 0 0

newtype GenT m a = GenT (RWST Module () GenState m a)
  deriving newtype (Functor, Applicative, Monad, Alternative)
  deriving newtype (MonadFail, MonadPlus, MonadReader Module)

instance Monad m => MonadFresh Hole (GenT m) where
  fresh = GenT . state $ \g@GenState { freshHole } ->
    (freshHole, g { freshHole = next freshHole })

instance Monad m => MonadFresh Free (GenT m) where
  fresh = GenT . state $ \g@GenState { freshFree } ->
    (freshFree, g { freshFree = next freshFree })

instance Monad m => MonadFresh Var (GenT m) where
  fresh = GenT . state $ \g@GenState { freshInt } ->
    (nVar freshInt, g { freshInt = next freshInt })

instance Monad m => MonadState (Map Hole HoleCtx) (GenT m) where
  state f = GenT . state $ \g@GenState { ctxs } ->
    let (x, s) = f ctxs in (x, g { ctxs = s })

runGenT :: Monad m => GenT m a -> Module -> GenState -> m (a, GenState)
runGenT (GenT m) env s = do
  (x, s', _) <- runRWST m env s
  return (x, s')

evalGenT :: Monad m => GenT m a -> Module -> GenState -> m a
evalGenT m env s = fst <$> runGenT m env s

genTree :: forall a. (a -> GenT [] a) -> a -> GenT Tree a
genTree = coerce (search @a @Module @() @GenState)
