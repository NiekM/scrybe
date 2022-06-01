module Nondet where

import Import
import Control.Applicative

newtype Nondet a = Nondet { runNondet :: Either String [a] }
  deriving stock (Functor, Foldable, Traversable)

instance Applicative Nondet where
  pure = Nondet . Right . pure
  liftA2 f (Nondet x) (Nondet y) = Nondet $ liftA2 (liftA2 f) x y

instance Monad Nondet where
  return = Nondet . Right . return
  Nondet x >>= k = Nondet $ x >>= fmap join . traverse (runNondet . k)

instance Alternative Nondet where
  empty = Nondet . Right $ []
  Nondet x <|> Nondet y = Nondet $ liftA2 (<|>) x y

instance MonadFail Nondet where
  fail = Nondet . Left

instance MonadPlus Nondet where

instance Pretty a => Pretty (Nondet a) where
  pretty = either pretty pretty . runNondet
