{-# LANGUAGE MultiParamTypeClasses #-}
module TermGen where

import Import
import Language
import Data.Tree
import Data.Coerce

data GenState = GenState
  { _holeCtxs  :: Map Hole HoleCtx
  , _env       :: Environment
  , _technique :: Technique
  , _concepts  :: MultiSet Concept
  , freshHole  :: Hole
  , freshFree  :: Free
  , freshVar   :: Int
  }

instance HasHoleCtxs GenState where
  holeCtxs = lens _holeCtxs \x y -> x { _holeCtxs = y }

instance HasEnvironment GenState where
  environment = lens _env \x y -> x { _env = y }

instance HasTechnique GenState where
  technique = lens _technique \x y -> x { _technique = y }

instance HasConcepts GenState where
  concepts = lens _concepts \x y -> x { _concepts = y }

mkGenState :: Environment -> Technique -> MultiSet Concept -> GenState
mkGenState e t c = GenState mempty e t c 0 0 0

newtype GenT m a = GenT (RWST Module () GenState m a)
  deriving newtype (Functor, Applicative, Monad, Alternative)
  deriving newtype (MonadFail, MonadPlus)
  deriving newtype (MonadState GenState, MonadReader Module)

instance Monad m => MonadFresh Hole (GenT m) where
  fresh = GenT . state $ \g@GenState { freshHole } ->
    (freshHole, g { freshHole = 1 + freshHole })

instance Monad m => MonadFresh Free (GenT m) where
  fresh = GenT . state $ \g@GenState { freshFree } ->
    (freshFree, g { freshFree = 1 + freshFree })

instance Monad m => MonadFresh Var (GenT m) where
  fresh = GenT . state $ \g@GenState { freshVar } ->
    (nVar freshVar, g { freshVar = 1 + freshVar })

runGenT :: Monad m => GenT m a -> Module -> GenState -> m (a, GenState)
runGenT (GenT m) pre s = do
  (x, s', _) <- runRWST m pre s
  return (x, s')

evalGenT :: Monad m => GenT m a -> Module -> GenState -> m a
evalGenT m pre s = fst <$> runGenT m pre s

genTree :: forall a. (a -> GenT [] a) -> a -> GenT Tree a
genTree = coerce (search @a @Module @() @GenState)
