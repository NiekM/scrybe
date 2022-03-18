{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Language.Syntax where

import Import hiding (reverse)
import Test.QuickCheck (Arbitrary(..), Gen, choose, sized, oneof, elements)
import Data.Foldable
import RIO.List (intersperse, repeat)
import RIO.NonEmpty (cons, reverse)
import Prettyprinter
import qualified RIO.Map as Map

-- Identifiers {{{

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

freeId :: Free -> Var
freeId (MkFree n) = MkVar . fromString . ('t':) . show $ n

-- }}}

-- Levels {{{

-- TODO: maybe add kinds?
-- TODO: term level might get extra parameters for e.g. type annotations
-- TODO: maybe level should contain values (concrete evaluation results) as
-- well as 'results' (as seen in Smyth)
data Level = Type | Term | Pattern | Value | Lambda
  deriving (Eq, Ord, Show, Read)

type family HasCtr' (l :: Level) where
  HasCtr' 'Lambda = 'False
  HasCtr' _       = 'True

type HasCtr l = HasCtr' l ~ 'True

type family HasVar' (l :: Level) where
  HasVar' 'Value = 'False
  HasVar' _      = 'True

type HasVar l = HasVar' l ~ 'True

type family HasApp' (l :: Level) where
  HasApp' _ = 'True

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

type HasArr l = (HasCtr l, HasApp l)
type NoBind l = (HasLam' l ~ 'False, HasCase' l ~ 'False, HasLet' l ~ 'False)

-- }}}

-- TODO: do we need a Core language and a surface language?
data Expr (l :: Level) a b where
  Hole :: b -> Expr l a b
  Ctr  :: HasCtr  l => Ctr -> Expr l a b
  Var  :: HasVar  l => a -> Expr l a b
  App  :: HasApp  l => Expr l a b -> Expr l a b -> Expr l a b
  Lam  :: HasLam  l => a -> Expr l a b -> Expr l a b
  Case :: HasCase l => Expr l a b -> [Branch l a b] -> Expr l a b
  Let  :: HasLet  l => a -> Expr l a b -> Expr l a b -> Expr l a b

deriving instance (Eq a, Eq b) => Eq (Expr l a b)
deriving instance (Ord a, Ord b) => Ord (Expr l a b)
deriving instance (Show a, Show b) => Show (Expr l a b)

pattern Arr :: () => HasArr l => Expr l a b -> Expr l a b -> Expr l a b
pattern Arr t u = App (App (Ctr (MkCtr "->")) t) u

-- Lenses {{{

holes' :: Traversal (Expr l a b) (Expr l a c) b (Expr l a c)
holes' g = go where
  go = \case
    Hole h -> g h
    Ctr c -> pure $ Ctr c
    Var v -> pure $ Var v
    App f x -> App <$> go f <*> go x
    Lam a x -> Lam a <$> go x
    Case x xs -> Case <$> go x <*> traverse (traverseOf arm go) xs
    Let a x y -> Let a <$> go x <*> go y

holes :: Traversal (Expr l a b) (Expr l a c) b c
holes = holes' . fmap (fmap Hole)

-- TODO: allow l to be changed in traversal
free' :: NoBind l => Traversal (Expr l a c) (Expr l b c) a (Expr l b c)
free' g = go where
  go = \case
    Hole h -> pure $ Hole h
    Ctr c -> pure $ Ctr c
    Var v -> g v
    App f x -> App <$> go f <*> go x

free :: (HasVar l, NoBind l) => Traversal (Expr l a c) (Expr l b c) a b
free = free' . fmap (fmap Var)

-- }}}

type Type = Expr 'Type Var
type Term = Expr 'Term Var
type Pattern = Expr 'Pattern Var
type Value = Expr 'Value Var Void

-- Substitution {{{

class Subst a b where
  subst :: Ord a => Map a b -> b -> b

joinHoles :: Expr l a (Expr l a b) -> Expr l a b
joinHoles = over holes' id

-- TODO: make these more generic without overlapping
instance Subst Hole (Expr l Var Hole) where
  subst th = joinHoles. over (holes' . fmap (fmap Hole))
    \x -> Map.findWithDefault (Hole x) x th

joinFree :: NoBind l => Expr l (Expr l a b) b -> Expr l a b
joinFree = over free' id

instance (HasVar l, NoBind l) => Subst a (Expr l a b) where
  subst th = joinFree . over (free' . fmap (fmap Var))
    \x -> Map.findWithDefault (Var x) x th

-- }}}

data Branch l a b = Branch (Expr 'Pattern a Void) (Expr l a b)
  deriving (Eq, Ord, Show)

pat :: Lens' (Branch l a b) (Expr 'Pattern a Void)
pat = lens (\(Branch p _) -> p) \(Branch _ a) p -> Branch p a

arm :: Lens (Branch l a b) (Branch l a c) (Expr l a b) (Expr l a c)
arm = lens (\(Branch _ a) -> a) \(Branch p _) a -> Branch p a

-- TODO: maybe move these definitions somewhere else

data Poly = Poly [Var] (Type Void)
  deriving (Eq, Ord, Show)

newtype Unit = Unit ()
  deriving newtype (Eq, Ord, Show, Read, Semigroup, Monoid)

data Sketch = Sketch (Term Unit) (Type Void)
  deriving (Eq, Ord, Show)

data HoleCtx = HoleCtx
  { goal :: Type Void
  , local :: Map Var VarId
  } deriving (Eq, Ord, Show)

-- TODO: replace this with a function that substitutes all goals and variables
-- within the gen monad.
substCtx :: Map Var (Type Void) -> HoleCtx -> HoleCtx
substCtx th ctx = ctx { goal = subst th $ goal ctx }

class HasHoleCtxs a where
  holeCtxs :: Lens' a (Map Hole HoleCtx)

-- TODO: what else do we need to track for local variables?
-- Variable name, type, number of holes it appears in, number of occurrences
data Variable = Variable Var (Type Void) Int Int
  deriving (Eq, Ord, Show)

substVar :: Map Var (Type Void) -> Variable -> Variable
substVar th (Variable v t i n) = Variable v (subst th t) i n

class HasVariables a where
  variables :: Lens' a (Map VarId Variable)

-- Module {{{

data Datatype = MkDatatype Ctr [Var] [(Ctr, [Type Void])]
  deriving (Eq, Ord, Show)

constructors :: Datatype -> [(Ctr, Poly)]
constructors (MkDatatype d as cs) = cs <&> second \ts ->
  Poly as (arrs (ts ++ [apps (Ctr d :| fmap Var as)]))

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

-- }}}

type FreshHole m = MonadFresh Hole m
type FreshFree m = MonadFresh Free m
type FreshVarId m = MonadFresh VarId m
type WithHoleCtxs s m = (MonadState s m, HasHoleCtxs s)
type WithVariables s m = (MonadState s m, HasVariables s)

-- TODO: replace with more general infix function
arrs :: (Foldable f, HasArr l) => f (Expr l a b) -> Expr l a b
arrs = foldr1 Arr

unArrs :: Expr l a b -> NonEmpty (Expr l a b)
unArrs = \case
  Arr t u -> t `cons` unArrs u
  t -> pure t

apps :: (Foldable f, HasApp l) => f (Expr l a b) -> Expr l a b
apps = foldl1 App

unApps :: Expr l a b -> NonEmpty (Expr l a b)
unApps = reverse . go where
  go = \case
    App f x -> x `cons` go f
    e -> pure e

lams :: (Foldable f, HasLam l) => f a -> Expr l a b -> Expr l a b
lams = flip (foldr Lam)

unLams :: Expr l a b -> ([a], Expr l a b)
unLams = \case
  Lam a x -> first (a:) $ unLams x
  e -> ([], e)

-- Pretty printing {{{

isArr :: Expr l a b -> Bool
isArr Arr {} = True
isArr _ = False

isLam :: Expr l a b -> Bool
isLam Lam {} = True
isLam _ = False

isApp :: Expr l a b -> Bool
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

instance (Pretty a, Pretty b) => Pretty (Branch l a b) where
  pretty (Branch c e) = pretty c <+> "->" <+> pretty e

instance (Pretty a, Pretty b) => Pretty (Expr l a b) where
  pretty = \case
    Hole i -> braces $ pretty i
    Var x -> pretty x
    Ctr c -> pretty c
    Arr t u -> sep [prettyParens isArr t, "->", pretty u]
    App f x -> sep
      [ prettyParens isLam f
      , prettyParens (\y -> isLam y || isApp y) x
      ]
    e@Lam {} ->
      let (as, x) = unLams e
      in "\\" <> sep (pretty <$> as) <+> "->" <+> pretty x
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
