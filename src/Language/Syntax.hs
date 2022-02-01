{-# LANGUAGE RankNTypes #-}
module Language.Syntax where

import Import
import GHC.TypeLits
import qualified RIO.Map as Map

newtype Free = Free Int
  deriving stock (Eq, Ord, Read, Show, Data)
  deriving newtype Num
  deriving Pretty via (NumVar "a")

newtype Hole = Hole Int
  deriving stock (Eq, Ord, Read, Show, Data)
  deriving newtype Num
  deriving Pretty via (NumVar "?")

newtype Bound = Bound Text
  deriving stock (Eq, Ord, Read, Show, Data)
  deriving newtype (IsString, Pretty)

-- * Types
data Type
  = TVar (Either Bound Free)
  | TApp Type Type
  deriving stock (Eq, Ord, Read, Show, Data)

pattern TArr :: Type -> Type -> Type
pattern TArr t u = TApp (TApp (TVar (Left (Bound "->"))) t) u

type Env = Map Text Type

-- * Expressions
data Expr a
  = ELam Binding (Expr a)
  | EApp (Expr a) (Expr a)
  | EVar (Either Bound Free)
  | EHole a
  deriving stock (Eq, Ord, Show, Read, Data)

-- TODO: this should probably also have hole contexts, maybe more.
data Sketch = Sketch
  { expr :: Expr Hole
  , goals :: Map Hole Type
  } deriving stock (Eq, Ord, Read, Show, Data)

data Binding = Binding Bound Type
  deriving stock (Eq, Ord, Read, Show, Data)

data Decl a = Decl Binding (Expr a)
  deriving stock (Eq, Ord, Read, Show, Data)

-- * Pretty printing
-- TODO: Use more generic pretty printing, then specialize to RIO.
-- TODO: Use quickCheck to show that parsing and printing are related.

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

isELam :: Expr a -> Bool
isELam ELam {} = True
isELam _ = False

isEApp :: Expr a -> Bool
isEApp EApp {} = True
isEApp _ = False

instance Pretty Sketch where
  pretty Sketch { expr, goals } = go expr where
    go = \case
      ELam (Binding x t) e ->
        "\\" <> pretty x <+> "::" <+> pretty t <> "." <+> go e
      EApp f x -> sep
        [ prettyParens f isELam
        , prettyParens x \y -> isELam y || isEApp y
        ]
      EVar x -> pretty x
      EHole i | Just x <- goals Map.!? i ->
        braces $ sep [pretty i, "::", pretty x]
      EHole i -> pretty i

instance Pretty a => Pretty (Expr a) where
  pretty = \case
    ELam (Binding x t) e ->
      "\\" <> pretty x <+> "::" <+> pretty t <> "." <+> pretty e
    EApp f x -> sep
      [ prettyParens f isELam
      , prettyParens x \y -> isELam y || isEApp y
      ]
    EVar x -> pretty x
    EHole i -> braces $ pretty i

instance Pretty Binding where
  pretty (Binding name ty) = pretty name <+> "::" <+> pretty ty
