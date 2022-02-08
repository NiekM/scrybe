{-# LANGUAGE DeriveTraversable, FlexibleInstances, DataKinds, GADTs #-}
module Language.Syntax where

import Import
import Test.QuickCheck
import RIO.List (intersperse)
import Prettyprinter

newtype Hole = MkHole Int
  deriving stock (Eq, Ord, Read, Show, Data)
  deriving newtype (Num, Pretty)

newtype Var = MkVar Text
  deriving stock (Eq, Ord, Read, Show, Data)
  deriving newtype (IsString, Pretty)

-- TODO: maybe add kinds?
-- TODO: term level might get extra parameters for e.g. type annotations
-- TODO: maybe level should contain values (concrete evaluation results) as
-- well as 'results' (as seen in Smyth)
data Level = Term | Type
  deriving (Eq, Ord, Show, Read)

-- TODO: make Binding generic in the annotation kind
data Binding a = Bind Var a
  deriving (Eq, Ord, Show, Read)
  deriving (Functor, Foldable, Traversable)

data Branch a = Branch { pat :: Var, arm :: a }
  deriving (Eq, Ord, Show, Read)
  deriving (Functor, Foldable, Traversable)

data Expr (l :: Level) a where
  Hole :: a -> Expr l a
  Var  :: Var -> Expr l a
  App  :: Expr l a -> Expr l a -> Expr l a
  -- Term specific
  Lam  :: Binding (Expr 'Type Hole) -> Expr 'Term a -> Expr 'Term a
  Case :: [Branch (Expr 'Term a)] -> Expr 'Term a

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
  Case xs <*> y = Case (fmap (fmap (<*> y)) xs)

instance Monad (Expr l) where
  Hole  h >>= k = k h
  Var   x >>= _ = Var x
  App f x >>= k = App (f >>= k) (x >>= k)
  Lam b f >>= k = Lam b (f >>= k)
  Case xs >>= k = Case (fmap (fmap (>>= k)) xs)

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

instance Pretty a => Pretty (Binding a) where
  pretty (Bind x t) = pretty x <+> "::" <+> pretty t

instance Pretty a => Pretty (Branch a) where
  pretty (Branch c e) = pretty c <+> "=>" <+> pretty e

instance Pretty a => Pretty (Expr l a) where
  pretty = \case
    Hole i -> braces $ pretty i
    Var x -> pretty x
    Arr t u -> sep [prettyParens t isArr, "->", pretty u]
    App f x -> sep
      [ prettyParens f isLam
      , prettyParens x \y -> isLam y || isApp y
      ]
    Lam b e -> "\\" <> pretty b <> "." <+> pretty e
    Case xs -> brackets . mconcat $ intersperse ";" (pretty <$> xs)

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

perms :: Int -> Int -> [[Int]]
perms _ 0 = [[]]
perms n 1 = [[n]]
perms n k = do
  x <- [1..(n - k + 1)]
  (x:) <$> perms (n - x) (k - 1)

sizedExp :: Arbitrary a => Int -> Gen (Term a)
sizedExp 0 = oneof [Var <$> arbitrary, Hole <$> arbitrary]
sizedExp n = frequency
  [ (5, do
    x <- choose (0, n - 1)
    let y = n - x - 1
    oneof
      [ App <$> sizedExp x <*> sizedExp y
      , Lam <$> (Bind <$> arbitrary <*> sizedTyp x) <*> sizedExp y
      ])
  , (1, do
    x <- choose (1, min n 5)
    xs <- oneof . fmap pure $ perms n x
    bs <- mapM (\i -> Branch <$> arbitrary <*> sizedExp (i - 1)) xs
    return $ Case bs)
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
sizedExprVoid n = frequency
  [ (5, do
    x <- choose (0, n - 1)
    let y = n - x - 1
    oneof
      [ App <$> sizedExprVoid x <*> sizedExprVoid y
      , Lam <$> (Bind <$> arbitrary <*> sizedTyp x) <*> sizedExprVoid y
      ])
  , (1, do
    x <- choose (1, min n 5)
    xs <- oneof . fmap pure $ perms n x
    bs <- mapM (\i -> Branch <$> arbitrary <*> sizedExprVoid (i - 1)) xs
    return $ Case bs)
  ]

instance Arbitrary (Term Void) where
  arbitrary = sized \n -> do
    m <- choose (0, n)
    sizedExprVoid m
