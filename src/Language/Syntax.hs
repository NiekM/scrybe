{-# LANGUAGE DeriveTraversable, FlexibleInstances #-}
module Language.Syntax where

import Import
import Test.QuickCheck

newtype Hole = Hole Int
  deriving stock (Eq, Ord, Read, Show, Data)
  deriving newtype (Num, Pretty)

newtype Var = Var Text
  deriving stock (Eq, Ord, Read, Show, Data)
  deriving newtype (IsString, Pretty)

-- * Types
data Type a
  = TVar Var
  | TApp (Type a) (Type a)
  | THole a
  deriving stock (Eq, Ord, Read, Show, Data)
  deriving stock (Functor, Foldable, Traversable)

instance Applicative Type where
  pure = THole
  TApp f x <*> y = TApp (f <*> y) (x <*> y)
  TVar   x <*> _ = TVar x
  THole  h <*> y = h <$> y

instance Monad Type where
  TApp f x >>= k = TApp (f >>= k) (x >>= k)
  TVar   x >>= _ = TVar x
  THole  h >>= k = k h

pattern TArr :: Type a -> Type a -> Type a
pattern TArr t u = TApp (TApp (TVar (Var "->")) t) u

type Env a = Map Var (Type a)

-- * Expressions
data Expr a
  = ELam (Binding Hole) (Expr a)
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

data Binding a = Binding Var (Type a)
  deriving stock (Eq, Ord, Read, Show, Data)

data Decl a = Decl (Binding Hole) (Expr a)
  deriving stock (Eq, Ord, Read, Show, Data)

-- * Small helper functions

isTArr :: Type a -> Bool
isTArr TArr {} = True
isTArr _ = False

isTApp :: Type a -> Bool
isTApp TApp {} = True
isTApp _ = False

isELam :: Expr a -> Bool
isELam ELam {} = True
isELam _ = False

isEApp :: Expr a -> Bool
isEApp EApp {} = True
isEApp _ = False

-- * Pretty printing

instance Pretty a => Pretty (Type a) where
  pretty = \case
    TVar x -> pretty x
    TArr t u -> sep [prettyParens t isTArr, "->", pretty u]
    TApp t u -> sep
      [ pretty t
      , prettyParens u isTApp
      ]
    THole i -> braces $ pretty i

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

instance Pretty a => Pretty (Binding a) where
  pretty (Binding name ty) = pretty name <+> "::" <+> pretty ty

-- * QuickCheck
-- TODO: make sure that these are good arbitrary instances

instance Arbitrary Var where
  arbitrary = fromString <$> resize 5 (listOf1 (chooseEnum ('a', 'z')))

instance Arbitrary Hole where
  arbitrary = fromIntegral <$> sized \n -> choose (0, n)

sizedType :: Int -> Gen (Type Hole)
sizedType 0 = TVar <$> arbitrary
sizedType n = do
  x <- choose (0, n - 1)
  TApp <$> sizedType x <*> sizedType (n - x - 1)

instance Arbitrary (Type Hole) where
  arbitrary = sized \n -> do
    m <- choose (0, n)
    sizedType m

instance Arbitrary (Binding Hole) where
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

instance Arbitrary (Expr (Type Hole)) where
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
