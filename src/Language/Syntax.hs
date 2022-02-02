{-# LANGUAGE DeriveTraversable, FlexibleInstances #-}
module Language.Syntax where

import Import
import qualified RIO.Map as Map
import Test.QuickCheck

newtype Hole = Hole Int
  deriving stock (Eq, Ord, Read, Show, Data)
  deriving newtype (Num, Pretty)

newtype Var = Var Text
  deriving stock (Eq, Ord, Read, Show, Data)
  deriving newtype (IsString, Pretty)

-- * Types
data Type
  = TVar Var
  | TApp Type Type
  | THole Hole
  deriving stock (Eq, Ord, Read, Show, Data)

pattern TArr :: Type -> Type -> Type
pattern TArr t u = TApp (TApp (TVar (Var "->")) t) u

type Env = Map Var Type

-- * Expressions
data Expr a
  = ELam Binding (Expr a)
  | EApp (Expr a) (Expr a)
  | EVar Var
  | EHole a
  deriving stock (Eq, Ord, Show, Read, Data)
  deriving stock (Functor, Foldable, Traversable)

instance Applicative Expr where
  pure = EHole
  ELam b x <*> y = ELam b (x <*> y)
  EApp f x <*> y = EApp (f <*> y) (x <*> y)
  EVar   x <*> _ = EVar x
  EHole  h <*> y = h <$> y

instance Monad Expr where
  ELam b f >>= k = ELam b (f >>= k)
  EApp f x >>= k = EApp (f >>= k) (x >>= k)
  EVar   x >>= _ = EVar x
  EHole  h >>= k = k h

-- TODO: this should probably also have hole contexts, maybe more.
data Sketch = Sketch
  { expr :: Expr Hole
  , goals :: Map Hole Type
  } deriving stock (Eq, Ord, Read, Show, Data)

data Binding = Binding Var Type
  deriving stock (Eq, Ord, Read, Show, Data)

data Decl a = Decl Binding (Expr a)
  deriving stock (Eq, Ord, Read, Show, Data)

-- * Small helper functions

isTArr :: Type -> Bool
isTArr TArr {} = True
isTArr _ = False

isTVar :: Type -> Bool
isTVar TVar {} = True
isTVar _ = False

isELam :: Expr a -> Bool
isELam ELam {} = True
isELam _ = False

isEApp :: Expr a -> Bool
isEApp EApp {} = True
isEApp _ = False

-- * Pretty printing

instance Pretty Type where
  pretty = \case
    TVar x -> pretty x
    TArr t u -> sep [prettyParens t isTArr, "->", pretty u]
    TApp t u -> sep [pretty t, prettyParens u (not . isTVar)]
    THole i -> braces $ pretty i

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

-- * QuickCheck
-- TODO: make sure that these are good arbitrary instances

instance Arbitrary Var where
  arbitrary = fromString <$> resize 5 (listOf1 (chooseEnum ('a', 'z')))

instance Arbitrary Hole where
  arbitrary = fromIntegral <$> sized \n -> choose (0, n)

sizedType :: Int -> Gen Type
sizedType 0 = TVar <$> arbitrary
sizedType n = do
  x <- choose (0, n - 1)
  TApp <$> sizedType x <*> sizedType (n - x - 1)

instance Arbitrary Type where
  arbitrary = sized \n -> do
    m <- choose (0, n)
    sizedType m

instance Arbitrary Binding where
  arbitrary = Binding <$> arbitrary <*> arbitrary

sizedExpr :: Arbitrary a => Int -> Gen (Expr a)
sizedExpr 0 = oneof [EVar <$> arbitrary, EHole <$> arbitrary]
sizedExpr n = do
  x <- choose (0, n - 1)
  let y = n - x - 1
  oneof
    [ EApp <$> sizedExpr x <*> sizedExpr y
    , ELam <$> (Binding <$> arbitrary <*> sizedType x) <*> sizedExpr y
    ]

arbExpr :: Arbitrary a => Gen (Expr a)
arbExpr = sized \n -> do
  m <- choose (0, n)
  sizedExpr m

instance Arbitrary (Expr Hole) where
  arbitrary = arbExpr

instance Arbitrary (Expr Type) where
  arbitrary = arbExpr

sizedExprVoid :: Int -> Gen (Expr Void)
sizedExprVoid 0 = oneof [EVar <$> arbitrary]
sizedExprVoid n = do
  x <- choose (0, n - 1)
  let y = n - x - 1
  oneof
    [ EApp <$> sizedExprVoid x <*> sizedExprVoid y
    , ELam <$> (Binding <$> arbitrary <*> sizedType x) <*> sizedExprVoid y
    ]

instance Arbitrary (Expr Void) where
  arbitrary = scale (`div` 3) $ sized \n -> do
    m <- choose (0, n)
    sizedExprVoid m
