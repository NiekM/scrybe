{-# LANGUAGE DeriveTraversable, FlexibleInstances, DataKinds, GADTs #-}
module Language.Syntax where

import Import
import Test.QuickCheck

newtype Hole = MkHole Int
  deriving stock (Eq, Ord, Read, Show, Data)
  deriving newtype (Num, Pretty)

newtype Var = MkVar Text
  deriving stock (Eq, Ord, Read, Show, Data)
  deriving newtype (IsString, Pretty)

-- TODO: maybe add kinds?
-- TODO: term level might get extra parameters for e.g. type annotations
data Level = Term | Type
  deriving (Eq, Ord, Show, Read)

data Expr (l :: Level) a where
  Hole :: a -> Expr l a
  Var  :: Var -> Expr l a
  App  :: Expr l a -> Expr l a -> Expr l a
  -- Level specific
  Lam  :: (Var, Expr 'Type Hole) -> Expr 'Term a -> Expr 'Term a

pattern Arr :: Expr l a -> Expr l a -> Expr l a
pattern Arr t u = App (App (Var (MkVar "->")) t) u

type Term = Expr 'Term
type Type = Expr 'Type

deriving instance Eq a => Eq (Expr l a)
deriving instance Ord a => Ord (Expr l a)
deriving instance Show a => Show (Expr l a)
deriving instance Functor (Expr l)
deriving instance Foldable (Expr l)
deriving instance Traversable (Expr l)

instance Applicative (Expr l) where
  pure = Hole
  Hole  h <*> y = h <$> y
  Var   x <*> _ = Var x
  App f x <*> y = App (f <*> y) (x <*> y)
  Lam b x <*> y = Lam b (x <*> y)

instance Monad (Expr l) where
  Hole  h >>= k = k h
  Var   x >>= _ = Var x
  App f x >>= k = App (f >>= k) (x >>= k)
  Lam b f >>= k = Lam b (f >>= k)

-- * Pretty printing

isArr :: Expr l a -> Bool
isArr Arr {} = True
isArr _ = False

isLam :: Expr l a -> Bool
isLam Lam {} = True
isLam _ = False

isApp :: Expr l a -> Bool
isApp App {} = True
isApp _ = False

instance Pretty a => Pretty (Expr l a) where
  pretty = \case
    Hole i -> braces $ pretty i
    Var x -> pretty x
    Arr t u -> sep [prettyParens t isArr, "->", pretty u]
    App f x -> sep
      [ prettyParens f isLam
      , prettyParens x \y -> isLam y || isApp y
      ]
    Lam (x, t) e ->
      "\\" <> pretty x <+> "::" <+> pretty t <> "." <+> pretty e

-- * QuickCheck
-- TODO: make sure that these are good arbitrary instances

sizedTyp :: Int -> Gen (Type Hole)
sizedTyp 0 = Var <$> arbitrary
sizedTyp n = do
  x <- choose (0, n - 1)
  App <$> sizedTyp x <*> sizedTyp (n - x - 1)

instance Arbitrary (Expr 'Type Hole) where
  arbitrary = sized \n -> do
    m <- choose (0, n)
    sizedTyp m

instance Arbitrary Var where
  arbitrary = fromString <$> resize 5 (listOf1 (chooseEnum ('a', 'z')))

instance Arbitrary Hole where
  arbitrary = fromIntegral <$> sized \n -> choose (0, n)

sizedExp :: Arbitrary a => Int -> Gen (Term a)
sizedExp 0 = oneof [Var <$> arbitrary, Hole <$> arbitrary]
sizedExp n = do
  x <- choose (0, n - 1)
  let y = n - x - 1
  oneof
    [ App <$> sizedExp x <*> sizedExp y
    , Lam <$> ((,) <$> arbitrary <*> sizedTyp x) <*> sizedExp y
    ]

arbExp :: Arbitrary a => Gen (Term a)
arbExp = sized \n -> do
  m <- choose (0, n)
  sizedExp m

instance Arbitrary (Term Hole) where
  arbitrary = arbExp

instance Arbitrary (Term (Type Hole)) where
  arbitrary = arbExp

sizedExprVoid :: Int -> Gen (Term Void)
sizedExprVoid 0 = oneof [Var <$> arbitrary]
sizedExprVoid n = do
  x <- choose (0, n - 1)
  let y = n - x - 1
  oneof
    [ App <$> sizedExprVoid x <*> sizedExprVoid y
    , Lam <$> ((,) <$> arbitrary <*> sizedTyp x) <*> sizedExprVoid y
    ]

instance Arbitrary (Term Void) where
  arbitrary = sized \n -> do
    m <- choose (0, n)
    sizedExprVoid m
