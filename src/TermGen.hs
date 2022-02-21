{-# LANGUAGE MultiParamTypeClasses #-}
module TermGen where

import Import hiding (local)
import Language
import Data.Tree
import Data.Coerce
import Control.Monad.RWS

data GenState = GenState
  { ctxs :: Map Hole HoleCtx
  , freshHole :: Hole
  , freshFree :: Free
  , freshVar :: Int
  }

emptyGenState :: GenState
emptyGenState = GenState mempty 0 0 0

-- TODO: The Module should probably be included in the GenState, since it might
-- be updated, such as reducing occurences.
newtype GenT m a = GenT (RWST Module () GenState m a)
  deriving newtype (Functor, Applicative, Monad, Alternative)
  deriving newtype (MonadFail, MonadPlus, MonadReader Module)

instance Monad m => MonadFresh Hole (GenT m) where
  fresh = GenT . state $ \g@GenState { freshHole } ->
    (freshHole, g { freshHole = 1 + freshHole })

instance Monad m => MonadFresh Free (GenT m) where
  fresh = GenT . state $ \g@GenState { freshFree } ->
    (freshFree, g { freshFree = 1 + freshFree })

instance Monad m => MonadFresh Var (GenT m) where
  fresh = GenT . state $ \g@GenState { freshVar } ->
    (nVar freshVar, g { freshVar = 1 + freshVar })

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
