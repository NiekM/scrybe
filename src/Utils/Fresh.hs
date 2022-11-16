module Utils.Fresh
  ( Fresh, fresh
  , Count(..)
  , TextVar(..)
  )
  where

import Control.Monad.RWS (MonadState(..))
import Prettyprinter (Pretty)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import RIO

newtype Fresh a = Fresh Natural
  deriving stock (Eq, Ord)
  deriving newtype (Show, Num, Real, Enum, Integral)

instance Semigroup (Fresh a) where (<>) = max
instance Monoid    (Fresh a) where mempty = 0

class Count a where
  fromNat :: Natural -> a
  default fromNat :: Num a => Natural -> a
  fromNat = fromIntegral

instance Count Natural

newtype TextVar (s :: Symbol) = TextVar Text
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (IsString, Pretty)

instance KnownSymbol s => Count (TextVar s) where
  fromNat = TextVar . fromString . (symbolVal (Proxy :: Proxy s) ++) . show

fresh :: (MonadState (Fresh a) m, Count a) => m a
fresh = do
  Fresh n <- get
  put $ Fresh (1 + n)
  return $ fromNat n
