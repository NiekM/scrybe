{-# LANGUAGE RankNTypes, DeriveTraversable, FlexibleInstances #-}
module Language.Syntax where

import Import
import GHC.TypeLits
import qualified RIO.Map as Map
import Test.QuickCheck

newtype Free = Free Int
  deriving stock (Eq, Ord, Read, Show, Data)
  deriving newtype Num
  deriving (Pretty, Arbitrary) via (NumVar "_")

newtype Hole = Hole Int
  deriving stock (Eq, Ord, Read, Show, Data)
  deriving newtype Num
  deriving (Pretty, Arbitrary) via (NumVar "?")

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

data Binding = Binding Bound Type
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

-- | A helper type for pretty printing and parsing variables
newtype NumVar (s :: Symbol) = MkNumVar Int
instance KnownSymbol s => Pretty (NumVar s) where
  pretty var@(MkNumVar n) = fromString (symbolVal var) <> fromString (show n)

instance Pretty Type where
  pretty = \case
    TArr t u -> sep [prettyParens t isTArr, "->", pretty u]
    TVar x -> pretty x
    TApp t u -> sep [pretty t, prettyParens u (not . isTVar)]

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

instance Arbitrary (NumVar s) where
  arbitrary = MkNumVar <$> sized \n -> chooseInt (0, n)

instance Arbitrary Bound where
  arbitrary = fromString <$> resize 5 (listOf1 (chooseEnum ('a', 'z')))

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
