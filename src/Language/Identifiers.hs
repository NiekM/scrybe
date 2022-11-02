{-# LANGUAGE MultiParamTypeClasses, RankNTypes #-}
{-# LANGUAGE TemplateHaskell#-}

module Language.Identifiers
  ( Hole(..), Free(..), Var(..), Ctr(..)
  , FreshHole, FreshFree, FreshVar
  , MonadFresh(..)
  , HasFreshState(..), FreshState()
  , Fresh, getFresh
  , Count()
  , mkFreshState
  , freshHole, freshFree, freshVar
  )
  where

import GHC.TypeLits
import Import
import Control.Monad.State

newtype Hole = MkHole Int
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (Num, Pretty)

-- NOTE: we assume that variables are always lowercase and constructors are
-- always uppercase.

newtype Ctr = MkCtr Text
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (IsString, Pretty)

newtype Free = MkFree Text
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (IsString, Pretty)

newtype Var = MkVar Text
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (IsString, Pretty)

newtype Fresh a = Fresh Int
  deriving stock (Eq, Ord)
  deriving newtype (Show, Num, Real, Enum, Integral)

class Count a where
  fromCounter :: Fresh a -> a
  default fromCounter :: Num a => Fresh a -> a
  fromCounter = fromIntegral

instance Count Hole

newtype TextVar (s :: Symbol) = MkTextVar Text
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (IsString, Pretty)

instance KnownSymbol s => Count (TextVar s) where
  fromCounter =
    MkTextVar . fromString . (symbolVal (Proxy :: Proxy s) ++) . show

deriving via TextVar "a" instance Count Var
deriving via TextVar "t" instance Count Free

getFresh :: (MonadState (Fresh a) m, Count a) => m a
getFresh = do
  n <- get
  put (1 + n)
  return $ fromCounter n




class Monad m => MonadFresh s m where
  fresh :: m s

type FreshHole m = MonadFresh Hole m
type FreshFree m = MonadFresh Free m
type FreshVar m = MonadFresh Var m

data FreshState = FreshState
  { _freshHole :: Fresh Hole
  , _freshFree :: Fresh Free
  , _freshVar  :: Fresh Var
  }

makeLenses 'FreshState

fresh' :: (MonadState s m, HasFreshState s, Num a) => Lens' FreshState a -> m a
fresh' l = freshState . l <<%= (+1)

fresh_ :: (Count a, MonadState s m, HasFreshState s) => Lens' FreshState (Fresh a) -> m a
fresh_ l = fromCounter <$> fresh' l

mkFreshState :: FreshState
mkFreshState = FreshState 0 0 0

class HasFreshState a where
  freshState :: Lens' a FreshState

instance HasFreshState FreshState where
  freshState = id

instance (Monoid w, HasFreshState s, Monad m)
  => MonadFresh Hole (RWST r w s m) where
  fresh = fresh_ freshHole
instance (Monoid w, HasFreshState s, Monad m)
  => MonadFresh Free (RWST r w s m) where
  fresh = fresh_ freshFree
instance (Monoid w, HasFreshState s, Monad m)
  => MonadFresh Var (RWST r w s m) where
  fresh = fresh_ freshVar

instance (HasFreshState s, Monad m) => MonadFresh Hole (StateT s m) where
  fresh = fresh_ freshHole
instance (HasFreshState s, Monad m) => MonadFresh Free (StateT s m) where
  fresh = fresh_ freshFree
instance (HasFreshState s, Monad m) => MonadFresh Var (StateT s m) where
  fresh = fresh_ freshVar
