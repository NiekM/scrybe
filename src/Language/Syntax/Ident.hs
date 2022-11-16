module Language.Syntax.Ident where

import Import

newtype Hole = MkHole Natural
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (Num, Pretty, NFData, Count)

newtype Free = MkFree Text
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (IsString, Pretty, NFData)
  deriving Count via TextVar "t"

-- NOTE: we assume that variables are always lowercase and constructors are--- always uppercase.

newtype Var = MkVar Text
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (IsString, Pretty, NFData)
  deriving Count via TextVar "a"

newtype Ctr = MkCtr Text
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (IsString, Pretty, NFData)
