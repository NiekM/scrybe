{-# options_ghc -Wno-orphans #-}

module Test where

import Import hiding (bool)
import Lang
import TermGen

import Data.Tree (levels)
import qualified RIO.Map as Map
import qualified RIO.Set as Set

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

-- | For each function signature, we compute all possible ways it can be
-- applied to holes.
instantiations :: Env -> Map Text [(Sketch, Type)]
instantiations = Map.mapWithKey \s t ->
  expand 0 (Sketch (EVar (Left (Bound s))) mempty) t

-- * "Hardcoded" synthesis of map
data Def = Def Text Type Sketch
  deriving stock (Eq, Ord, Read, Show)

instance Pretty Def where
  pretty (Def name ty sketch) =
    pretty name <+> "::" <+> pretty ty
    <> linebreak
    <> pretty name <+> "=" <+> pretty sketch

mapSketch :: Sketch
mapSketch = Sketch
  { expr = ELam "f" (TArr "a" "b") (EHole 0)
  , goals = Map.singleton 0 (TArr (list "a") (list "b"))
  }

mapDef :: Def
mapDef = Def
  "map"
  (TArr (TArr "a" "b") (TArr (list "a") (list "b")))
  mapSketch

holeContexts :: Env -> Expr -> Map Hole Env
holeContexts env = \case
  ELam (Bound x) t e -> holeContexts (Map.insert x t env) e
  EApp x y -> Map.unionsWith Map.intersection
    [ holeContexts env x
    , holeContexts env y
    ]
  EHole i -> Map.singleton i env
  _ -> Map.empty

fromSketch :: Env -> Sketch -> GenState
fromSketch env sketch@Sketch{ expr } = GenState
  { sketch
  , contexts
  , maxHole = 1 + fromMaybe 0 (Set.lookupMax $ Map.keysSet contexts)
  , maxFree = 0
  } where contexts = holeContexts env expr

test :: [[GenState]]
test = levels $ genTree step (fromSketch prelude mapSketch)


