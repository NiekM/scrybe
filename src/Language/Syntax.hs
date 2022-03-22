{-# LANGUAGE RankNTypes, PolyKinds, UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Language.Syntax where

import Import hiding (reverse)
import qualified Data.Kind as Kind
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

type family IsJust (t :: Maybe a) where
  IsJust ('Just _) = 'True
  IsJust 'Nothing  = 'False

type NoBind l =
  ( HasLam' l ~ 'False
  , HasCase' l ~ 'False
  , HasLet' l ~ 'False
  )

data Func a = Fix | Base a
  deriving (Eq, Ord, Show, Read)

type family Rec (f :: Func Kind.Type) (l :: Level) b where
  Rec 'Fix l b = Expr' 'Fix l b
  Rec ('Base c) _ _ = c

type family VAR (l :: Level) where
  VAR _ = Var

-- }}}

-- TODO: do we need a Core language and a surface language?
data Expr' (r :: Func Kind.Type) (l :: Level) b where
  Hole :: b -> Expr' r l b
  Ctr :: HasCtr l => Ctr -> Expr' r l b
  Var :: HasVar l => VAR l -> Expr' r l b
  App :: HasApp l => Rec r l b -> Rec r l b -> Expr' r l b
  Lam :: HasLam l => VAR l -> Rec r l b -> Expr' r l b
  Let :: HasLet l => VAR l -> Rec r l b -> Rec r l b -> Expr' r l b
  Case :: HasCase l => Rec r l b -> [(Ctr, Rec r l b)] -> Expr' r l b

type Expr = Expr' 'Fix
type Base a = Expr' ('Base a)

deriving instance (Eq b, Eq (Rec r l b)) => Eq (Expr' r l b)
deriving instance (Ord b, Ord (Rec r l b)) => Ord (Expr' r l b)
deriving instance (Show b, Show (Rec r l b)) => Show (Expr' r l b)

pattern Arr :: () => HasArr l => Expr l b -> Expr l b -> Expr l b
pattern Arr t u = App (App (Ctr (MkCtr "->")) t) u

type Type = Expr 'Type
type Term = Expr 'Term
type Pattern = Expr 'Pattern
type Value = Expr 'Value Void

-- Morphisms {{{

traverseExpr :: Applicative f => (Rec r l b -> f (Rec r' l b)) ->
  Expr' r l b -> f (Expr' r' l b)
traverseExpr go = \case
  Hole h -> pure $ Hole h
  Ctr c -> pure $ Ctr c
  Var v -> pure $ Var v
  App f x -> App <$> go f <*> go x
  Lam a x -> Lam a <$> go x
  Let a x y -> Let a <$> go x <*> go y
  Case x xs -> Case <$> go x <*> traverse (traverse go) xs

mapExpr :: (Rec r l b -> Rec r' l b) -> Expr' r l b -> Expr' r' l b
mapExpr go = runIdentity . traverseExpr (Identity . go)

para :: (Expr l b -> Base c l b -> c) -> Expr l b -> c
para g e = g e (mapExpr (para g) e)

cata :: (Base c l b -> c) -> Expr l b -> c
cata = para . const

apo :: (c -> Either (Expr l b) (Base c l b)) -> c -> Expr l b
apo g e = either id (mapExpr (apo g)) (g e)

ana :: (c -> Base c l b) -> c -> Expr l b
ana = apo . (return .)

coerceExpr ::
  ( a ~ HasCtr l, a' ~ HasCtr l', a => a'
  , b ~ HasVar l, b' ~ HasVar l', b => b'
  , c ~ HasApp l, c' ~ HasApp l', c => c'
  , d ~ HasLam l, d' ~ HasLam l', d => d'
  , e ~ HasLet l, e' ~ HasLet l', e => e'
  , f ~ HasCase l, f' ~ HasCase l', f => f'
  ) => Expr l y -> Expr l' y
coerceExpr = cata \case
  Hole h -> Hole h
  Ctr c -> Ctr c
  Var v -> Var v
  App f x -> App f x
  Lam a x -> Lam a x
  Let a x y -> Let a x y
  Case x xs -> Case x xs

fixExpr :: Base (Expr l b) l b -> Expr l b
fixExpr = \case
  Hole h -> Hole h
  Ctr c -> Ctr c
  Var v -> Var v
  App f x -> App f x
  Lam a x -> Lam a x
  Let a x y -> Let a x y
  Case x xs -> Case x xs

-- }}}

-- Lenses {{{

holes' ::
  ( a ~ HasCtr l, a' ~ HasCtr l', a => a'
  , b ~ HasVar l, b' ~ HasVar l', b => b'
  , c ~ HasApp l, c' ~ HasApp l', c => c'
  , d ~ HasLam l, d' ~ HasLam l', d => d'
  , e ~ HasLet l, e' ~ HasLet l', e => e'
  , f ~ HasCase l, f' ~ HasCase l', f => f'
  ) => Traversal (Expr l x) (Expr l' y) x (Expr l' y)
holes' g = cata \case
  Hole h -> g h
  Ctr c -> pure $ Ctr c
  Var v -> pure $ Var v
  App f x -> App <$> f <*> x
  Lam a x -> Lam a <$> x
  Let a x y -> Let a <$> x <*> y
  Case x xs -> Case <$> x <*> traverse sequenceA xs

holes :: Traversal (Expr l b) (Expr l c) b c
holes = holes' . fmap (fmap Hole)

free' :: (a ~ HasCtr l, a' ~ HasCtr l', a => a', NoBind l)
  => NoBind l => Traversal (Expr l c) (Expr l' c) (VAR l) (Expr l' c)
free' g = cata \case
  Hole h -> pure $ Hole h
  Ctr c -> pure $ Ctr c
  Var v -> g v
  App f x -> App <$> f <*> x

free :: (a ~ HasCtr l, a' ~ HasCtr l', a => a', HasVar l', NoBind l)
  => Traversal (Expr l c) (Expr l' c) (VAR l) (VAR l')
free = free' . fmap (fmap Var)

-- }}}

-- Substitution {{{

subst :: (Ord (VAR l), expr ~ Expr l b) => Map (VAR l) expr -> expr -> expr
subst th = cata \case
  Var v | Just x <- Map.lookup v th -> x
  e -> fixExpr e

fill :: (Ord b, expr ~ Expr l b) => Map b expr -> expr -> expr
fill th = cata \case
  Hole h | Just x <- Map.lookup h th -> x
  e -> fixExpr e

-- }}}

-- TODO: maybe move these definitions somewhere else

data Poly = Poly [Var] (Type Void)
  deriving (Eq, Ord, Show)

newtype Unit = Unit ()
  deriving newtype (Eq, Ord, Show, Read, Semigroup, Monoid)

data HoleCtx = HoleCtx (Type Void) (Map Var VarId)
  deriving (Eq, Ord, Show)

goal :: Lens' HoleCtx (Type Void)
goal = lens (\(HoleCtx t _) -> t) \(HoleCtx _ vs) t -> HoleCtx t vs

local :: Lens' HoleCtx (Map Var VarId)
local = lens (\(HoleCtx _ vs) -> vs) \(HoleCtx t _) vs -> HoleCtx t vs

class HasHoleCtxs a where
  holeCtxs :: Lens' a (Map Hole HoleCtx)

-- TODO: what else do we need to track for local variables?
-- Variable name, type, number of holes it appears in, number of occurrences
data Variable = Variable Var (Type Void) Int Int
  deriving (Eq, Ord, Show)

varType :: Lens' Variable (Type Void)
varType = lens (\(Variable _ t _ _) -> t) \(Variable x _ i n) t ->
  Variable x t i n

class HasVariables a where
  variables :: Lens' a (Map VarId Variable)

data Signature = MkSignature Var Poly
  deriving (Eq, Ord, Show)

data Binding a = MkBinding Var (Term a)
  deriving (Eq, Ord, Show)

data Datatype = MkDatatype Ctr [Var] [(Ctr, [Type Void])]
  deriving (Eq, Ord, Show)

data Sketch = Sketch Var Poly (Term Unit)
  deriving (Eq, Ord, Show)

newtype Sigs = Sigs [Signature]
  deriving (Eq, Ord, Show)

-- Module {{{

constructors :: Datatype -> [(Ctr, Poly)]
constructors (MkDatatype d as cs) = cs <&> second \ts ->
  Poly as (arrs (ts ++ [apps (Ctr d :| fmap Var as)]))

data Definition a
  = Signature Signature
  | Binding (Binding a)
  | Datatype Datatype
  deriving (Eq, Ord, Show)

newtype Module a = Module [Definition a]
  deriving (Eq, Ord, Show)

sepModule :: Module a -> ([Signature], [Binding a], [Datatype])
sepModule (Module ds) = foldr go mempty ds where
  go = \case
    Signature s -> over _1 (s:)
    Binding   b -> over _2 (b:)
    Datatype  d -> over _3 (d:)

ctrs :: Module a -> Map Ctr Poly
ctrs (Module xs) = Map.fromList $ xs >>= \case
  Datatype d -> constructors d
  _ -> []

sigs :: Module a -> Map Var Poly
sigs (Module xs) = Map.fromList $ xs >>= \case
  Signature (MkSignature x t) -> [(x, t)]
  _ -> []

binds :: Module a -> Map Var (Term a)
binds (Module xs) = Map.fromList $ xs >>= \case
  Binding (MkBinding x t) -> [(x, t)]
  _ -> []

functions :: Module a -> Map Var (Term a, Poly)
functions m = Map.intersectionWith (,) (binds m) (sigs m)

-- }}}

type FreshHole m = MonadFresh Hole m
type FreshFree m = MonadFresh Free m
type FreshVarId m = MonadFresh VarId m
type WithHoleCtxs s m = (MonadState s m, HasHoleCtxs s)
type WithVariables s m = (MonadState s m, HasVariables s)

-- Helper functions {{{

-- TODO: replace with more general infix function
arrs :: (Foldable f, HasArr l) => f (Expr l b) -> Expr l b
arrs = foldr1 Arr

unArrs :: Expr l b -> NonEmpty (Expr l b)
unArrs = \case
  Arr t u -> t `cons` unArrs u
  t -> pure t

apps :: (Foldable f, HasApp l) => f (Expr l b) -> Expr l b
apps = foldl1 App

unApps :: Expr l b -> NonEmpty (Expr l b)
unApps = reverse . go where
  go = \case
    App f x -> x `cons` go f
    e -> pure e

lams :: (Foldable f, HasLam l) => f (VAR l) -> Expr l b -> Expr l b
lams = flip (foldr Lam)

unLams :: Expr l b -> ([VAR l], Expr l b)
unLams = \case
  Lam a x -> first (a:) $ unLams x
  e -> ([], e)

-- }}}

-- Pretty printing {{{

isArr :: Expr l b -> Bool
isArr Arr {} = True
isArr _ = False

isLam :: Expr l b -> Bool
isLam Lam {} = True
isLam _ = False

isApp :: Expr l b -> Bool
isApp App {} = True
isApp _ = False

prettyParens :: Pretty a => (a -> Bool) -> a -> Doc ann
prettyParens p t
  | p t = parens (pretty t)
  | otherwise = pretty t

instance Pretty Unit where
  pretty _ = space

instance Pretty Poly where
  pretty = \case
    Poly [] t -> pretty t
    Poly xs t -> "forall" <+> sep (pretty <$> xs) <> dot <+> pretty t

instance Pretty b => Pretty (Expr l b) where
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
    Let a x e -> "let" <+> pretty a <+> "=" <+> pretty x <+> "in" <+> pretty e
    Case x xs -> "case" <+> pretty x <+> "of" <+>
      mconcat (intersperse "; " $ xs <&> \(p, a) ->
        let (as, b) = unLams a
        in sep (pretty p : fmap pretty as) <+> "->" <+> pretty b)

instance Pretty Datatype where
  pretty (MkDatatype d as cs) =
    "data" <+> sep (pretty d : fmap pretty as) <+>
      ( align . sep . zipWith (<+>) ("=" : repeat "|")
      $ cs <&> \(c, xs) -> pretty (apps (Ctr c :| xs))
      )

instance Pretty Signature where
  pretty (MkSignature x t) = pretty x <+> "::" <+> pretty t

instance Pretty a => Pretty (Binding a) where
  pretty (MkBinding x e) = sep (pretty x : fmap pretty as) <+> "=" <+> pretty b
    where (as, b) = unLams e

instance Pretty Sketch where
  pretty (Sketch x s b) = vsep
    [pretty x <+> "::" <+> pretty s, pretty x <+> "=" <+> pretty b]

instance Pretty a => Pretty (Definition a) where
  pretty = \case
    Signature s -> pretty s
    Binding b -> pretty b
    Datatype d -> pretty d

instance Pretty a => Pretty (Module a) where
  pretty (Module cs) = vsep $ fmap pretty cs

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
