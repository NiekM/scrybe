{-# LANGUAGE DataKinds, GADTs, RankNTypes #-}
{-# LANGUAGE TypeFamilies, ConstraintKinds #-}
module Language.Syntax where

import Import hiding (reverse)
import Test.QuickCheck (Arbitrary(..), Gen, choose, sized, oneof, elements)
import Data.Foldable
import RIO.List (intersperse, repeat)
import RIO.NonEmpty (cons, reverse)
import Prettyprinter
import qualified RIO.Map as Map

newtype Hole = MkHole Int
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (Num, Pretty)

newtype Free = MkFree Int
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (Num, Pretty)

newtype VarId = MkVarId Int
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (Num, Pretty)

newtype Var = MkVar Text
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (IsString, Pretty)

newtype Ctr = MkCtr Text
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (IsString, Pretty)

varId :: VarId -> Var
varId (MkVarId n) = MkVar . fromString . ('a':) . show $ n

-- Levels {{{

-- TODO: maybe add kinds?
-- TODO: term level might get extra parameters for e.g. type annotations
-- TODO: maybe level should contain values (concrete evaluation results) as
-- well as 'results' (as seen in Smyth)
data Level = Type | Term | Pattern | Value
  deriving (Eq, Ord, Show, Read)

type family HasVar' (l :: Level) where
  HasVar' 'Value   = 'False
  HasVar' 'Pattern = 'False
  HasVar' _        = 'True

type HasVar l = HasVar' l ~ 'True

type family HasApp' (l :: Level) where
  HasApp' 'Value = 'False
  HasApp' _      = 'True

type HasApp l = HasApp' l ~ 'True

type family HasLam' (l :: Level) where
  HasLam' 'Term = 'True
  HasLam' _     = 'False

type HasLam l = HasLam' l ~ 'True

type family HasCase' (l :: Level) where
  HasCase' 'Term = 'True
  HasCase' _     = 'False

type HasCase l = HasCase' l ~ 'True

type family HasLet' (l :: Level) where
  HasLet' 'Term = 'True
  HasLet' _     = 'False

type HasLet l = HasLet' l ~ 'True

-- }}}

data Expr (l :: Level) a where
  Hole :: a -> Expr l a
  Ctr  :: Ctr -> Expr l a

  Var  :: HasVar  l => Var -> Expr l a
  App  :: HasApp  l => Expr l a -> Expr l a -> Expr l a
  Lam  :: HasLam  l => Var -> Expr l a -> Expr l a
  Case :: HasCase l => Expr l a -> [Branch (Expr l a)] -> Expr l a
  Let  :: HasLet  l => Var -> Expr l a -> Expr l a -> Expr l a

pattern Arr :: () => HasApp l => Expr l a -> Expr l a -> Expr l a
pattern Arr t u = App (App (Ctr (MkCtr "->")) t) u

type Type = Expr 'Type
type Term = Expr 'Term
type Pattern = Expr 'Pattern
type Value = Expr 'Value Void

data Poly = Poly [Free] (Type Free)
  deriving (Eq, Ord, Show)

data Def = Def Var Poly (Term Void)
  deriving (Eq, Ord, Show)

data Branch a = Branch { pat :: Pattern Var, arm :: a }
  deriving (Eq, Ord, Show)
  deriving (Functor, Foldable, Traversable)

newtype Unit = Unit ()
  deriving newtype (Eq, Ord, Show, Read, Semigroup, Monoid)

data Sketch = Sketch (Term Unit) (Type Free)
  deriving (Eq, Ord, Show)

data HoleCtx = HoleCtx
  { goal :: Type Free
  , local :: Map Var VarId
  } deriving (Eq, Ord, Show)

class HasHoleCtxs a where
  holeCtxs :: Lens' a (Map Hole HoleCtx)

-- TODO: replace this with a function that substitutes all goals and variables
-- within the gen monad.
substCtx :: Map Free (Type Free) -> HoleCtx -> HoleCtx
substCtx th ctx = ctx { goal = subst th $ goal ctx }

-- TODO: what else do we need to track for local variables?
-- Variable name, type, number of holes it appears in, number of occurrences
data Variable = Variable Var (Type Free) Int Int
  deriving (Eq, Ord, Show)

substVar :: Map Free (Type Free) -> Variable -> Variable
substVar th (Variable v t i n) = Variable v (subst th t) i n

class HasVariables a where
  variables :: Lens' a (Map VarId Variable)

data Datatype = MkDatatype Ctr [Free] [(Ctr, [Type Free])]
  deriving (Eq, Ord, Show)

constructors :: Datatype -> [(Ctr, Poly)]
constructors (MkDatatype d as cs) = cs <&> second \ts ->
  Poly as (arrs (ts ++ [apps (Ctr d :| fmap Hole as)]))

data Definition
  = Signature Var Poly
  | Binding Var (Term Void)
  | Datatype Datatype
  deriving (Eq, Ord, Show)

newtype Module = Module [Definition]
  deriving (Eq, Ord, Show)

ctrs :: Module -> Map Ctr Poly
ctrs (Module xs) = Map.fromList $ xs >>= \case
  Datatype d -> constructors d
  _ -> []

sigs :: Module -> Map Var Poly
sigs (Module xs) = Map.fromList $ xs >>= \case
  Signature x t -> [(x, t)]
  _ -> []

binds :: Module -> Map Var (Term Void)
binds (Module xs) = Map.fromList $ xs >>= \case
  Binding x t -> [(x, t)]
  _ -> []

functions :: Module -> Map Var (Term Void, Poly)
functions m = Map.intersectionWith (,) (binds m) (sigs m)

type FreshHole m = MonadFresh Hole m
type FreshFree m = MonadFresh Free m
type FreshVarId m = MonadFresh VarId m
type WithHoleCtxs s m = (MonadState s m, HasHoleCtxs s)
type WithVariables s m = (MonadState s m, HasVariables s)

-- TODO: replace with more general infix function
arrs :: (Foldable f, HasApp l) => f (Expr l a) -> Expr l a
arrs = foldr1 Arr

unArrs :: Expr l a -> NonEmpty (Expr l a)
unArrs = \case
  Arr t u -> t `cons` unArrs u
  t -> pure t

apps :: (Foldable f, HasApp l) => f (Expr l a) -> Expr l a
apps = foldl1 App

unApps :: Expr l a -> NonEmpty (Expr l a)
unApps = reverse . go where
  go = \case
    App f x -> x `cons` go f
    e -> pure e

lams :: (Foldable f, HasLam l) =>
  f Var -> Expr l a -> Expr l a
lams = flip (foldr Lam)

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
  Ctr c <*> _ = Ctr c
  App f x <*> y = App (f <*> y) (x <*> y)
  Lam b x <*> y = Lam b (x <*> y)
  Case x xs <*> y = Case (x <*> y) (fmap (<*> y) <$> xs)
  Let a x e <*> y = Let a (x <*> y) (e <*> y)

instance Monad (Expr l) where
  Hole h >>= k = k h
  Var x >>= _ = Var x
  Ctr c >>= _ = Ctr c
  App f x >>= k = App (f >>= k) (x >>= k)
  Lam b x >>= k = Lam b (x >>= k)
  Case x xs >>= k = Case (x >>= k) (fmap (>>= k) <$> xs)
  Let a x e >>= k = Let a (x >>= k) (e >>= k)

-- }}}

-- Pretty printing {{{

isArr :: Expr l a -> Bool
isArr Arr {} = True
isArr _ = False

isLam :: Expr l a -> Bool
isLam Lam {} = True
isLam _ = False

isApp :: Expr l a -> Bool
isApp App {} = True
isApp _ = False

prettyParens :: Pretty a => (a -> Bool) -> a -> Doc ann
prettyParens p t
  | p t = parens (pretty t)
  | otherwise = pretty t

instance Pretty Unit where
  pretty _ = space

instance Pretty Sketch where
  pretty (Sketch def sig) = pretty def <+> "::" <+> pretty sig

instance Pretty Poly where
  pretty = \case
    Poly [] t -> pretty t
    Poly xs t -> "forall" <+> sep (pretty <$> xs) <> dot <+> pretty t

instance Pretty Def where
  pretty (Def x t e) = pretty x <+> "::" <+> pretty t <+> "=" <+> pretty e

instance Pretty a => Pretty (Branch a) where
  pretty (Branch c e) = pretty c <+> "=>" <+> pretty e

instance Pretty a => Pretty (Expr l a) where
  pretty = \case
    Hole i -> braces $ pretty i
    Var x -> pretty x
    Ctr c -> pretty c
    Arr t u -> sep [prettyParens isArr t, "->", pretty u]
    App f x -> sep
      [ prettyParens isLam f
      , prettyParens (\y -> isLam y || isApp y) x
      ]
    Lam b e -> "\\" <> pretty b <> "." <+> pretty e
    Case x xs -> "case" <+> pretty x <+> "of" <+>
      mconcat (intersperse "; " $ pretty <$> xs)
    Let a x e -> "let" <+> pretty a <+> "=" <+> pretty x <+> "in" <+> pretty e

instance Pretty Datatype where
  pretty (MkDatatype d as cs) =
    "data" <+> sep (pretty d : fmap pretty as) <+>
      ( align . sep . zipWith (<+>) ("=" : repeat "|")
      $ cs <&> \(c, xs) -> pretty (apps (Ctr c :| xs))
      )

instance Pretty Definition where
  pretty = \case
    Signature x t -> pretty x <+> "::" <+> pretty t
    Binding x e -> pretty x <+> "=" <+> pretty e
    Datatype d -> pretty d

instance Pretty Module where
  pretty (Module cs) = vsep . fmap pretty $ cs
-- }}}

-- QuickCheck {{{
-- TODO: replace these instances with more sensible ones, probably defined
-- using a naive synthesizer

-- sizedTyp :: Int -> Gen (Type Free)
-- sizedTyp 0 = Var <$> arbitrary
-- sizedTyp n = do
--   x <- choose (0, n - 1)
--   App <$> sizedTyp x <*> sizedTyp (n - x - 1)

-- instance Arbitrary (Expr 'Type Free) where
--   arbitrary = sized \n -> do
--     m <- choose (0, n)
--     sizedTyp m

-- instance Arbitrary Var where
--   arbitrary = fromString . return <$> elements ['a'..'z']

-- instance Arbitrary Ctr where
--   arbitrary = fromString . return <$> elements ['A'..'Z']

-- instance Arbitrary Hole where
--   arbitrary = fromIntegral <$> sized \n -> choose (0, n)

-- perms :: Int -> Int -> [[Int]]
-- perms _ 0 = [[]]
-- perms n 1 = [[n]]
-- perms n k = do
--   x <- [1..(n - k + 1)]
--   (x:) <$> perms (n - x) (k - 1)

-- sizedExp :: [Gen (Term a)] -> Int -> Gen (Term a)
-- sizedExp as 0 = oneof as
-- sizedExp as n = do
--   x <- choose (1, min n 5)
--   xs <- oneof . fmap pure $ perms n x
--   case map (subtract 1) xs of
--     [] -> error "unreachable"
--     ys@(z:zs) -> oneof
--       [ Case <$> _ <*> mapM (\i -> Branch <$> arbitrary <*> sizedExp as i) ys
--       , apps <$> mapM (sizedExp as) ys
--       -- TODO: don't throw away size parameter
--       , lams <$> mapM (const arbitrary) zs
--         <*> sizedExp as z
--       ]

-- arbExp :: Arbitrary a => Gen (Term a)
-- arbExp = sized \n -> do
--   m <- choose (0, n)
--   sizedExp [Var <$> arbitrary, Ctr <$> arbitrary, Hole <$> arbitrary] m

-- instance Arbitrary (Term Hole) where
--   arbitrary = arbExp

-- instance Arbitrary (Term (Type Free)) where
--   arbitrary = arbExp

-- instance Arbitrary (Term Void) where
--   arbitrary = sized \n -> do
--     m <- choose (0, n)
--     sizedExp [Var <$> arbitrary, Ctr <$> arbitrary] m

-- }}}
