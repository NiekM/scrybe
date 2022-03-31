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

-- NOTE: we assume that variables are always lowercase and constructors are
-- always uppercase.

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

-- TODO: do we need a Core language and a surface language?
-- TODO: maybe add kinds?
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

-- }}}

-- | The type of expressions, generic in
--
-- r: the type of recursion, used to differentiate between base functors and
-- their fixed point, as well as allowing recursive calls to be interleaved
-- with annotations.
--
-- l: the level of the expression, containing both universe levels as well as
-- different compiler stages. This parameter is used to determine which
-- constructors are available.
--
-- v: the type of variables used in variable references as well as bindings.
--
-- h: the type of holes used in the expression.
--
data Expr' (r :: Func Kind.Type) (l :: Level) v h where
  Hole :: h -> Expr' r l v h
  Ctr :: HasCtr l => Ctr -> Expr' r l v h
  Var :: HasVar l => v -> Expr' r l v h
  App :: HasApp l => Rec r l v h -> Rec r l v h -> Expr' r l v h
  Lam :: HasLam l => v -> Rec r l v h -> Expr' r l v h
  Let :: HasLet l => v -> Rec r l v h -> Rec r l v h -> Expr' r l v h
  Case :: HasCase l => Rec r l v h -> [(Ctr, Rec r l v h)] -> Expr' r l v h

data Func a = Fix | Base a | Ann a
  deriving (Eq, Ord, Show, Read)

-- TODO: perhaps we should remove Fix and just have Ann, as it would make
-- things much simpler with generalizing functions, but it would be slightly
-- less efficient. For optional annotations, it might be better to have an
-- actual Ann constructor in Expr', as well as some functions converting from
-- `Ann a l v h -> Expr' ('Annotate l a) v h` and
-- `Expr' ('Annotate l a) v h -> Ann (Maybe a) l v h`.
type family Rec (f :: Func Kind.Type) (l :: Level) v h where
  Rec 'Fix l v h = Expr l v h
  Rec ('Base c) _ _ _ = c
  Rec ('Ann a) l v h = Ann a l v h

type Expr = Expr' 'Fix
type Base a = Expr' ('Base a)
type Ann a l v h = Annot (Expr' ('Ann a) l v h) a

deriving instance (Eq v, Eq h, Eq (Rec r l v h)) => Eq (Expr' r l v h)
deriving instance (Ord v, Ord h, Ord (Rec r l v h)) => Ord (Expr' r l v h)
deriving instance (Show v, Show h, Show (Rec r l v h)) => Show (Expr' r l v h)

pattern Arr :: () => HasArr l => Expr l v h -> Expr l v h -> Expr l v h
pattern Arr t u = App (App (Ctr (MkCtr "->")) t) u

type Type = Expr 'Type Var Void
type Term = Expr 'Term
type Pattern = Expr 'Pattern
type Value = Expr 'Value Void

-- Morphisms {{{

rec :: Traversal (Expr' r l v h) (Expr' r' l v h) (Rec r l v h) (Rec r' l v h)
rec go = \case
  Hole h -> pure $ Hole h
  Ctr c -> pure $ Ctr c
  Var v -> pure $ Var v
  App f x -> App <$> go f <*> go x
  Lam a x -> Lam a <$> go x
  Let a x y -> Let a <$> go x <*> go y
  Case x xs -> Case <$> go x <*> traverse (traverse go) xs

paraExpr :: (Expr l v h -> Base c l v h -> c) -> Expr l v h -> c
paraExpr g e = g e (over rec (paraExpr g) e)

cataExpr :: (Base c l v h -> c) -> Expr l v h -> c
cataExpr = paraExpr . const

apoExpr :: (c -> Either (Expr l v h) (Base c l v h)) -> c -> Expr l v h
apoExpr g e = either id (over rec (apoExpr g)) (g e)

anaExpr :: (c -> Base c l v h) -> c -> Expr l v h
anaExpr = apoExpr . (return .)

coerceExpr ::
  ( a ~ HasCtr l, a' ~ HasCtr l', a => a'
  , b ~ HasVar l, b' ~ HasVar l', b => b'
  , c ~ HasApp l, c' ~ HasApp l', c => c'
  , d ~ HasLam l, d' ~ HasLam l', d => d'
  , e ~ HasLet l, e' ~ HasLet l', e => e'
  , f ~ HasCase l, f' ~ HasCase l', f => f'
  ) => Expr l v h -> Expr l' v h
coerceExpr = cataExpr \case
  Hole h -> Hole h
  Ctr c -> Ctr c
  Var v -> Var v
  App f x -> App f x
  Lam a x -> Lam a x
  Let a x y -> Let a x y
  Case x xs -> Case x xs

fixExpr :: Base (Expr l v h) l v h -> Expr l v h
fixExpr = over rec id

mapAnn :: (a -> b) -> Ann a l v h -> Ann b l v h
mapAnn f (Annot e a) = Annot (over rec (mapAnn f) e) (f a)

paraAnn :: (Ann a l v h -> Base c l v h -> c) -> Ann a l v h -> c
paraAnn g (Annot e a) = g (Annot e a) (over rec (paraAnn g) e)

cataAnn :: (a -> Base c l v h -> c) -> Ann a l v h -> c
cataAnn = paraAnn . (. view ann)

-- }}}

-- Lenses {{{

holes' :: Traversal (Expr l v h) (Expr l v h') h (Expr l v h')
holes' g = cataExpr \case
  Hole h -> g h
  Ctr c -> pure $ Ctr c
  Var v -> pure $ Var v
  App f x -> App <$> f <*> x
  Lam a x -> Lam a <$> x
  Let a x y -> Let a <$> x <*> y
  Case x xs -> Case <$> x <*> traverse sequenceA xs

holes :: Traversal (Expr l v h) (Expr l v h') h h'
holes = holes' . fmap (fmap Hole)

free' :: NoBind l => Traversal (Expr l v h) (Expr l v' h) v (Expr l v' h)
free' g = cataExpr \case
  Hole h -> pure $ Hole h
  Ctr c -> pure $ Ctr c
  Var v -> g v
  App f x -> App <$> f <*> x

free :: (NoBind l, HasVar l) => Traversal (Expr l v h) (Expr l v' h) v v'
free = free' . fmap (fmap Var)

-- }}}

-- Substitution {{{

subst :: (Ord v, expr ~ Expr l v h) => Map v expr -> expr -> expr
subst th = cataExpr \case
  Var v | Just x <- Map.lookup v th -> x
  e -> fixExpr e

fill :: (Ord h, expr ~ Expr l v h) => Map h expr -> expr -> expr
fill th = cataExpr \case
  Hole h | Just x <- Map.lookup h th -> x
  e -> fixExpr e

-- }}}

-- TODO: maybe move these definitions somewhere else

data Annot x a = Annot x a
  deriving (Eq, Ord, Show)

ann :: Lens' (Annot x a) a
ann = lens (\(Annot _ a) -> a) \(Annot x _) a -> Annot x a

data Poly = Poly [Var] Type
  deriving (Eq, Ord, Show)

-- | Turn a monotype into a polytype by quantifying all its free variables.
poly :: Type -> Poly
poly t = Poly (nubOrd $ toListOf free t) t

alphaEq :: Poly -> Poly -> Maybe (Map Var Var)
alphaEq (Poly as t) (Poly bs u) = do
  let cs = filter (`elem` as) . nubOrd $ toListOf free t
  let th = Map.fromList $ zip bs cs
  guard $ t == subst (Var <$> th) u
  return th

-- TODO: replace freezing with something more reasonable
-- | Turn a polytype into a monotype by turning all quantified variables in
-- constructors of the same name.
freeze :: Poly -> Type
freeze (Poly as t) = flip cataExpr t \case
  Var v | v `elem` as, MkVar c <- v -> Ctr (MkCtr c)
  e -> fixExpr e

newtype Unit = Unit ()
  deriving newtype (Eq, Ord, Show, Read, Semigroup, Monoid)

data HoleCtx = HoleCtx Type (Map Var VarId)
  deriving (Eq, Ord, Show)

goal :: Lens' HoleCtx Type
goal = lens (\(HoleCtx t _) -> t) \(HoleCtx _ vs) t -> HoleCtx t vs

local :: Lens' HoleCtx (Map Var VarId)
local = lens (\(HoleCtx _ vs) -> vs) \(HoleCtx t _) vs -> HoleCtx t vs

class HasHoleCtxs a where
  holeCtxs :: Lens' a (Map Hole HoleCtx)

-- TODO: what else do we need to track for local variables?
-- Variable name, type, number of holes it appears in, number of occurrences
data Variable = Variable Var Type Int Int
  deriving (Eq, Ord, Show)

varType :: Lens' Variable Type
varType = lens (\(Variable _ t _ _) -> t) \(Variable x _ i n) t ->
  Variable x t i n

class HasVariables a where
  variables :: Lens' a (Map VarId Variable)

data Signature = MkSignature Var Poly
  deriving (Eq, Ord, Show)

data Binding v h = MkBinding Var (Term v h)
  deriving (Eq, Ord, Show)

data Datatype = MkDatatype Ctr [Var] [(Ctr, [Type])]
  deriving (Eq, Ord, Show)

data Sketch = Sketch Var Poly (Term Var Unit)
  deriving (Eq, Ord, Show)

newtype Sigs = Sigs [Signature]
  deriving (Eq, Ord, Show)

-- Module {{{

constructors :: Datatype -> [(Ctr, Poly)]
constructors (MkDatatype d as cs) = cs <&> second \ts ->
  Poly as (arrs (ts ++ [apps (Ctr d :| fmap Var as)]))

data Definition a
  = Signature Signature
  | Binding (Binding Var a)
  | Datatype Datatype
  deriving (Eq, Ord, Show)

newtype Module a = Module [Definition a]
  deriving (Eq, Ord, Show)

sepModule :: Module a -> ([Signature], [Binding Var a], [Datatype])
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

binds :: Module a -> Map Var (Term Var a)
binds (Module xs) = Map.fromList $ xs >>= \case
  Binding (MkBinding x t) -> [(x, t)]
  _ -> []

functions :: Module a -> Map Var (Term Var a, Poly)
functions m = Map.intersectionWith (,) (binds m) (sigs m)

-- }}}

type FreshHole m = MonadFresh Hole m
type FreshFree m = MonadFresh Free m
type FreshVarId m = MonadFresh VarId m
type WithHoleCtxs s m = (MonadState s m, HasHoleCtxs s)
type WithVariables s m = (MonadState s m, HasVariables s)

-- Helper functions {{{

-- TODO: replace with more general infix function
arrs :: (Foldable f, HasArr l) => f (Expr l v h) -> Expr l v h
arrs = foldr1 Arr

unArrs :: Expr l v h -> NonEmpty (Expr l v h)
unArrs = \case
  Arr t u -> t `cons` unArrs u
  t -> pure t

apps :: (Foldable f, HasApp l) => f (Expr l v h) -> Expr l v h
apps = foldl1 App

unApps :: Expr l v h -> NonEmpty (Expr l v h)
unApps = reverse . go where
  go = \case
    App f x -> x `cons` go f
    e -> pure e

lams :: (Foldable f, HasLam l) => f v -> Expr l v h -> Expr l v h
lams = flip (foldr Lam)

unLams :: Expr l v h -> ([v], Expr l v h)
unLams = \case
  Lam a x -> first (a:) $ unLams x
  e -> ([], e)

strip :: Ann a l v h -> Expr l v h
strip = cataAnn (const fixExpr)

collect :: Ann a l v h -> [Annot (Expr l v h) a]
collect = paraAnn \a t -> Annot (strip a) (view ann a) : view rec t

-- }}}

-- Pretty printing {{{

isArr :: Expr l v h -> Bool
isArr Arr {} = True
isArr _ = False

isLam :: Expr l v h -> Bool
isLam Lam {} = True
isLam _ = False

isApp :: Expr l v h -> Bool
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

instance (Pretty a, Pretty (Annot a b)) => Pretty (Annot a (Maybe b)) where
  pretty (Annot x Nothing) = pretty x
  pretty (Annot x (Just a)) = pretty $ Annot x a

instance Pretty a => Pretty (Annot a Type) where
  pretty (Annot x t) = pretty x <+> "::" <+> pretty t

instance Pretty a => Pretty (Annot a Text) where
  pretty (Annot x p) = "{-#" <+> pretty p <+> "#-}" <+> pretty x

instance (Pretty (Ann a 'Term v h), Pretty v, Pretty h)
  => Pretty (Expr' ('Ann a) 'Term v h) where
  pretty = \case
    Hole i -> braces $ pretty i
    Var x -> pretty x
    Ctr c -> pretty c
    App f x -> parens (pretty f) <+> parens (pretty x)
    Lam a x -> parens $ "\\" <> pretty a <+> "->" <+> parens (pretty x)
    Let a x e -> "let" <+> pretty a <+> "=" <+> pretty x <+> "in" <+> pretty e
    Case x xs -> "case" <+> pretty x <+> "of" <+>
      mconcat (intersperse "; " $ xs <&> \(p, a) ->
        pretty p <+> "->" <+> pretty a)

instance (Pretty v, Pretty h) => Pretty (Expr l v h) where
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

instance (Pretty v, Pretty h) => Pretty (Binding v h) where
  pretty (MkBinding x e) = sep (pretty x : fmap pretty as) <+> "=" <+> pretty b
    where (as, b) = unLams e

instance Pretty Sketch where
  pretty (Sketch x s b) = vsep
    [pretty $ MkSignature x s, pretty $ MkBinding x b]

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
