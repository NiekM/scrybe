{-# LANGUAGE RankNTypes #-}
module Lang where

import Import
import GHC.TypeLits
import qualified RIO.Map as Map

newtype TFree = TFree Int
  deriving stock (Eq, Ord, Read, Show, Data)
  deriving newtype Num
  deriving Pretty via (NumVar "a")

newtype EFree = EFree Int
  deriving stock (Eq, Ord, Read, Show, Data)
  deriving newtype Num
  deriving Pretty via (NumVar "x")

newtype Hole = Hole Int
  deriving stock (Eq, Ord, Read, Show, Data)
  deriving newtype Num
  deriving Pretty via (NumVar "?")

newtype Bound = Bound Text
  deriving stock (Eq, Ord, Read, Show, Data)
  deriving newtype (IsString, Pretty)

-- * Types
data Type
  = TVar (Either Bound TFree)
  | TApp Type Type
  deriving stock (Eq, Ord, Read, Show, Data)

pattern TArr :: Type -> Type -> Type
pattern TArr t u = TApp (TApp (TVar (Left (Bound "->"))) t) u

type Env = Map Text Type

-- * Expressions
data Expr
  = ELam Bound Type Expr
  | EApp Expr Expr
  | EVar (Either Bound TFree)
  | EHole Hole
  deriving stock (Eq, Ord, Show, Read, Data)

-- TODO: this should probably also have hole contexts, maybe more.
data Sketch = Sketch
  { expr :: Expr
  , goals :: Map Hole Type
  } deriving stock (Eq, Ord, Read, Show, Data)

-- * Utility functions

-- | Generates all possible ways to apply holes to an expression
expand :: Hole -> Sketch -> Type -> [(Sketch, Type)]
expand n sketch@(Sketch e ts) t = (sketch, t) : case t of
  TArr t1 t2 ->
    expand (1 + n) (Sketch (EApp e (EHole n)) (Map.insert n t1 ts)) t2
  _ -> []

-- * Pretty printing

-- | A helper type for pretty printing variables
newtype NumVar (s :: Symbol) = MkNumVar Int
instance KnownSymbol s => Pretty (NumVar s) where
  pretty var@(MkNumVar n) = fromString (symbolVal var) <> fromString (show n)

prettyParens :: Pretty a => a -> (a -> Bool) -> StyleDoc
prettyParens t p
  | p t = parens (pretty t)
  | otherwise = pretty t

isTArr :: Type -> Bool
isTArr TArr {} = True
isTArr _ = False

isTVar :: Type -> Bool
isTVar TVar {} = True
isTVar _ = False

instance Pretty Type where
  pretty = \case
    TArr t u -> sep [prettyParens t isTArr, "->", pretty u]
    TVar x -> pretty x
    TApp t u -> sep [pretty t, prettyParens u (not . isTVar)]

isELam :: Expr -> Bool
isELam ELam {} = True
isELam _ = False

isEApp :: Expr -> Bool
isEApp EApp {} = True
isEApp _ = False

instance Pretty Sketch where
  pretty Sketch { expr, goals } = go expr where
    go = \case
      ELam x t e -> "\\" <> pretty x <+> "::" <+> pretty t <> "." <+> go e
      EApp f x -> sep
        [ prettyParens f isELam
        , prettyParens x \y -> isELam y || isEApp y
        ]
      EVar x -> pretty x
      EHole i | Just x <- goals Map.!? i ->
        braces $ sep [pretty i, "::", pretty x]
      EHole i -> pretty i

instance Pretty Expr where
  pretty e = pretty (Sketch e mempty)
