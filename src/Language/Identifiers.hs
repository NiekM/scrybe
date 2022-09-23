{-# LANGUAGE MultiParamTypeClasses, RankNTypes #-}
module Language.Identifiers
  ( Hole(..), Free(..), Var(..), Ctr(..)
  , VarId(), varId
  , FreeId(), freeId
  , FreshHole, FreshFree, FreshVar
  , MonadFresh(..)
  , HasFreshState(..), FreshState()
  , Fresh, getFresh
  , mkFreshState
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

newtype FreeId = MkFreeId Int
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (Num, Pretty)

newtype Free = MkFree Text
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (IsString, Pretty)

newtype VarId = MkVarId Int
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (Num, Pretty)

newtype Var = MkVar Text
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (IsString, Pretty)

varId :: VarId -> Var
varId (MkVarId n) = MkVar . fromString . ('a':) . show $ n

freeId :: FreeId -> Free
freeId (MkFreeId n) = MkFree . fromString . ('t':) . show $ n

class Monad m => MonadFresh s m where
  fresh :: m s

type FreshHole m = MonadFresh Hole m
type FreshFree m = MonadFresh Free m
type FreshVar m = MonadFresh Var m

data FreshState = FreshState
  { _freshHole :: Hole
  , _freshFree :: FreeId
  , _freshVar  :: VarId
  }

fresh' :: (MonadState s m, HasFreshState s, Num a) => Lens' FreshState a -> m a
fresh' l = freshState . l <<%= (+1)

freshHole :: Lens' FreshState Hole
freshHole = lens _freshHole \x y -> x { _freshHole = y }

freshFree :: Lens' FreshState FreeId
freshFree = lens _freshFree \x y -> x { _freshFree = y }

freshVar :: Lens' FreshState VarId
freshVar = lens _freshVar \x y -> x { _freshVar = y }

mkFreshState :: FreshState
mkFreshState = FreshState 0 0 0

class HasFreshState a where
  freshState :: Lens' a FreshState

instance HasFreshState FreshState where
  freshState = id

instance (Monoid w, HasFreshState s, Monad m)
  => MonadFresh Hole (RWST r w s m) where
  fresh = fresh' freshHole
instance (Monoid w, HasFreshState s, Monad m)
  => MonadFresh Free (RWST r w s m) where
  fresh = freeId <$> fresh' freshFree
instance (Monoid w, HasFreshState s, Monad m)
  => MonadFresh Var (RWST r w s m) where
  fresh = varId <$> fresh' freshVar

instance (HasFreshState s, Monad m) => MonadFresh Hole (StateT s m) where
  fresh = fresh' freshHole
instance (HasFreshState s, Monad m) => MonadFresh Free (StateT s m) where
  fresh = freeId <$> fresh' freshFree
instance (HasFreshState s, Monad m) => MonadFresh Var (StateT s m) where
  fresh = varId <$> fresh' freshVar

newtype Fresh a = Fresh Int
  deriving (Eq, Ord, Show)
  deriving newtype (Num, Real, Enum, Integral)

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

getFresh :: Count a => State (Fresh a) a
getFresh = do
  n <- get
  put (1 + n)
  return $ fromCounter n
