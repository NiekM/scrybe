{-# LANGUAGE DeriveTraversable, FlexibleInstances, DataKinds, GADTs #-}
{-# LANGUAGE TypeFamilies, ConstraintKinds #-}
module Language.Syntax where

import Import hiding (reverse)
import Test.QuickCheck
import Data.Foldable
import RIO.List (intersperse)
import RIO.NonEmpty (cons, reverse)
import Prettyprinter

newtype Hole = MkHole Int
  deriving stock (Eq, Ord, Read, Show, Data)
  deriving newtype (Num, Pretty)

newtype Var = MkVar Text
  deriving stock (Eq, Ord, Read, Show, Data)
  deriving newtype (IsString, Pretty)

newtype Ctr = MkCtr Text
  deriving stock (Eq, Ord, Read, Show, Data)
  deriving newtype (IsString, Pretty)

-- Levels {{{

-- TODO: maybe add kinds?
-- TODO: term level might get extra parameters for e.g. type annotations
-- TODO: maybe level should contain values (concrete evaluation results) as
-- well as 'results' (as seen in Smyth)
data Level = Term | Type | Pattern
  deriving (Eq, Ord, Show, Read)

type family HasLam (l :: Level) where
  HasLam 'Term = 'True
  HasLam _     = 'False

type Lam l = HasLam l ~ 'True

type family HasCase (l :: Level) where
  HasCase 'Term = 'True
  HasCase _     = 'False

type Case l = HasCase l ~ 'True

type family HasApp (l :: Level) where
  HasApp 'Term = 'True
  HasApp 'Type = 'True
  HasApp _     = 'False

type App l = HasApp l ~ 'True

-- }}}

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
  Ctr  :: Ctr -> [Expr l a] -> Expr l a

  App  :: App  l => Expr l a -> Expr l a -> Expr l a
  Lam  :: Lam  l => Binding (Expr 'Type Hole) -> Expr l a -> Expr l a
  Case :: Case l => [Branch (Expr l a)] -> Expr l a

pattern Arr :: () => App l => Expr l a -> Expr l a -> Expr l a
pattern Arr t u = App (App (Var (MkVar "->")) t) u

type Term = Expr 'Term
type Type = Expr 'Type

-- Instances {{{

deriving instance Eq a => Eq (Expr l a)
deriving instance Ord a => Ord (Expr l a)
deriving instance Show a => Show (Expr l a)
deriving instance Functor (Expr l)
deriving instance Foldable (Expr l)
deriving instance Traversable (Expr l)

instance Applicative (Expr l) where
  pure = Hole
  Hole h <*> y = h <$> y
  Var x <*> _ = Var x
  Ctr c xs <*> y = Ctr c (fmap (<*> y) xs)
  App f x <*> y = App (f <*> y) (x <*> y)
  Lam b x <*> y = Lam b (x <*> y)
  Case xs <*> y = Case (fmap (fmap (<*> y)) xs)

instance Monad (Expr l) where
  Hole h >>= k = k h
  Var x >>= _ = Var x
  Ctr c xs >>= k = Ctr c (fmap (>>= k) xs)
  App f x >>= k = App (f >>= k) (x >>= k)
  Lam b f >>= k = Lam b (f >>= k)
  Case xs >>= k = Case (fmap (fmap (>>= k)) xs)

-- }}}

-- Pretty printing {{{

isArr :: Expr l a -> Bool
isArr Arr {} = True
isArr _ = False

isLam :: Expr l a -> Bool
isLam Lam {} = True
isLam _ = False

isCtr :: Expr l a -> Bool
isCtr Ctr {} = True
isCtr _ = False

isApp :: Expr l a -> Bool
isApp App {} = True
isApp Ctr {} = True
isApp _ = False

prettyParens :: Pretty a => (a -> Bool) -> a -> Doc ann
prettyParens p t
  | p t = parens (pretty t)
  | otherwise = pretty t

instance Pretty a => Pretty (Binding a) where
  pretty (Bind x t) = pretty x <+> "::" <+> pretty t

instance Pretty a => Pretty (Branch a) where
  pretty (Branch c e) = pretty c <+> "=>" <+> pretty e

instance Pretty a => Pretty (Expr l a) where
  pretty = \case
    Hole i -> braces $ pretty i
    Var x -> pretty x
    Ctr c xs -> sep (pretty c
      : map (prettyParens (\x -> isLam x || isApp x || isArr x)) xs)
    Arr t u -> sep [prettyParens isArr t, "->", pretty u]
    App f x -> sep
      [ prettyParens (\y -> isLam y || isCtr y) f
      , prettyParens (\y -> isLam y || isApp y) x
      ]
    Lam b e -> "\\" <> pretty b <> "." <+> pretty e
    Case xs -> brackets . mconcat $ intersperse ";" (pretty <$> xs)

-- }}}

-- QuickCheck {{{
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
  arbitrary = fromString . return <$> chooseEnum ('a', 'z')

instance Arbitrary Ctr where
  arbitrary = fromString . return <$> chooseEnum ('A', 'Z')

instance Arbitrary Hole where
  arbitrary = fromIntegral <$> sized \n -> choose (0, n)

perms :: Int -> Int -> [[Int]]
perms _ 0 = [[]]
perms n 1 = [[n]]
perms n k = do
  x <- [1..(n - k + 1)]
  (x:) <$> perms (n - x) (k - 1)

apps :: (Foldable f, App l) => f (Expr l a) -> Expr l a
apps = foldl1 App

unApps :: Expr l a -> NonEmpty (Expr l a)
unApps = reverse . go where
  go = \case
    App f x -> x `cons` go f
    e -> pure e

lams :: (Foldable f, Lam l) => f (Binding (Type Hole)) -> Expr l a -> Expr l a
lams = flip (foldr Lam)

sizedExp :: [Gen (Term a)] -> Int -> Gen (Term a)
sizedExp as 0 = oneof as
sizedExp as n = do
  x <- choose (1, min n 5)
  xs <- oneof . fmap pure $ perms n x
  case map (subtract 1) xs of
    [] -> error "unreachable"
    ys@(z:zs) -> oneof
      [ Case <$> mapM (\i -> Branch <$> arbitrary <*> sizedExp as i) ys
      , Ctr <$> arbitrary <*> mapM (sizedExp as) ys
      , apps <$> mapM (sizedExp as) ys
      , lams <$> mapM (\i -> Bind <$> arbitrary <*> sizedTyp i) zs
        <*> sizedExp as z
      ]

arbExp :: Arbitrary a => Gen (Term a)
arbExp = sized \n -> do
  m <- choose (0, n)
  sizedExp [Var <$> arbitrary, Hole <$> arbitrary] m

instance Arbitrary (Term Hole) where
  arbitrary = arbExp

instance Arbitrary (Term (Type Hole)) where
  arbitrary = arbExp

instance Arbitrary (Term Void) where
  arbitrary = sized \n -> do
    m <- choose (0, n)
    sizedExp [Var <$> arbitrary] m

-- }}}
