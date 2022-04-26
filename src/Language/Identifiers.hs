{-# LANGUAGE MultiParamTypeClasses, RankNTypes #-}
module Language.Identifiers
  ( Hole(..), Free(..), Var(..), Ctr(..)
  , FreshHole, FreshFree, FreshVar
  , MonadFresh(..)
  , HasFreshState(..), FreshState()
  , mkFreshState
  )
  where

import Import

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
