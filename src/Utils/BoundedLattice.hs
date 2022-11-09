{-# LANGUAGE DeriveGeneric #-}

module Utils.BoundedLattice where

import RIO hiding (and, or)

class BoundedLattice a where
  top, bot :: a
  top = conj []
  bot = disj []
  and, or :: a -> a -> a
  and x y = conj [x,y]
  or x y = disj [x,y]
  conj, disj :: [a] -> a
  conj = foldl' and top
  disj = foldl' or bot

data Logic a
  = Pure a
  | Conjunction [Logic a]
  | Disjunction [Logic a]
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving stock (Functor, Foldable, Traversable)

instance NFData a => NFData (Logic a)

-- TODO: check that these instances make sense
instance Applicative Logic where
  pure = Pure
  Pure f <*> x = f <$> x
  Conjunction fs <*> x = Conjunction $ fs <&> (<*> x)
  Disjunction fs <*> x = Disjunction $ fs <&> (<*> x)

instance Monad Logic where
  Pure x >>= f = f x
  Conjunction xs >>= f = Conjunction $ xs <&> (>>= f)
  Disjunction xs >>= f = Disjunction $ xs <&> (>>= f)

instance BoundedLattice (Logic a) where
  conj = Conjunction
  disj = Disjunction

dnf :: Logic a -> [[a]]
dnf = \case
  Pure a -> [[a]]
  Conjunction xs -> concat <$> mapM dnf xs
  Disjunction xs -> xs >>= dnf
