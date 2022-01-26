{-# options_ghc -Wno-orphans #-}

module Test where

import Import hiding (bool)
import Lang
import TermGen

import Data.Tree (levels)
import Data.Map.Strict (singleton)

instance IsString Expr where fromString = EVar . Right . fromString
instance IsString Type where fromString = TVar . Right . fromString

list :: Type -> Type
list = TApp "List"

nat, bool :: Type
nat = "Nat"
bool = "Bool"

a, b, c :: Type
a = TVar (Left 0)
b = TVar (Left 1)
c = TVar (Left 2)

data Sig = Sig Text Type
  deriving stock (Eq, Ord, Show, Read)

data Dec = Dec Sig Expr
  deriving stock (Eq, Ord, Show, Read)

instance Pretty Sig where
  pretty (Sig x t) = sep [pretty x, "::", pretty t]

instance Pretty Dec where
  pretty (Dec s@(Sig x _) e) =
    pretty s <> linebreak <> sep [pretty x, "=", pretty e]

pattern (:=) :: Text -> Type -> Sig
pattern a := t = Sig a t
infix 4 :=

prelude :: [Sig]
prelude =
  [ "T"       := bool
  , "F"       := bool
  , "Nil"     := list a
  , "Cons"    := a `TArr` (list a `TArr` list a)
  , "compose" := (b `TArr` c) `TArr` ((a `TArr` b) `TArr` (a `TArr` c))
  , "foldr"   := (a `TArr` (b `TArr` b)) `TArr` (b `TArr` (list a `TArr` b))
  ]

-- | For each function signature, we compute all possible ways it can be
-- applied to holes.
instantiations :: [Sig] -> [[(Sketch, Type)]]
instantiations = map \(Sig s t) ->
  expand 0 (Sketch (EVar (Right (Free s))) mempty) t

-- * "Hardcoded" synthesis of map
-- TODO: compute initial GenState from a sketch.

mapSketch :: Dec
mapSketch = Dec
  ("map" := TArr (TArr "a" "b") (TArr (list "a") (list "b")))
  (ELam 0 (EHole 0))

genMap :: GenState
genMap = GenState
  { sketch = Sketch
    { expr  = EHole 0
    , goals = singleton 0 (TArr (list "a") (list "b"))
    }
  , options = instantiations prelude
    <> [[(Sketch (EVar (Left 0)) mempty, TArr "a" "b")]]
  , maxHole = 1
  , maxTBound = 2
  }

test :: [[GenState]]
test = levels $ genTree step genMap


