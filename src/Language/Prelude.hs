{-# options_ghc -Wno-orphans #-}

module Language.Prelude where

import Import hiding (bool)
import Language.Syntax

import qualified RIO.Map as Map

instance IsString Expr where fromString = EVar . Left . fromString
instance IsString Type where fromString = TVar . Left . fromString

list :: Type -> Type
list = TApp "List"

nat, bool :: Type
nat = "Nat"
bool = "Bool"

a, b, c :: Type
a = TVar (Right 0)
b = TVar (Right 1)
c = TVar (Right 2)

pattern (:=) :: Text -> Type -> (Text, Type)
pattern a := t = (a, t)
infix 4 :=

prelude :: Env
prelude = Map.fromList
  [ "T"       := bool
  , "F"       := bool
  , "Nil"     := list a
  , "Cons"    := a `TArr` (list a `TArr` list a)
  , "compose" := (b `TArr` c) `TArr` ((a `TArr` b) `TArr` (a `TArr` c))
  , "foldr"   := (a `TArr` (b `TArr` b)) `TArr` (b `TArr` (list a `TArr` b))
  ]
