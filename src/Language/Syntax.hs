{-# LANGUAGE RankNTypes, PolyKinds, UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Language.Syntax
  ( module Language.Syntax
  , module Language.Identifiers
  )
  where

import Import hiding (reverse)
import Language.Identifiers
import qualified Data.Kind as Kind
import Data.Foldable
import RIO.List (intersperse, repeat)
import RIO.NonEmpty (cons, reverse)
import Prettyprinter
import qualified RIO.Map as Map

-- Levels {{{

-- TODO: do we need a Core language and a surface language?
-- TODO: maybe add kinds?
-- TODO: maybe level should contain values (concrete evaluation results) as
-- well as 'results' (as seen in Smyth)
data Level
  = Type   -- ^ Type level expressions
  | Term   -- ^ Term level expressions
  | Lambda -- ^ Lambda expressions
  | Det    -- ^ Determinate results
  | Indet  -- ^ Indeterminate results
  | Value  -- ^ Concrete values
  deriving (Eq, Ord, Show, Read)

type family HasCtr' (l :: Level) where
  HasCtr' 'Lambda = 'False
  HasCtr' 'Indet  = 'False
  HasCtr' _       = 'True

type HasCtr l = HasCtr' l ~ 'True

type family HasVar' (l :: Level) where
  HasVar' 'Value = 'False
  HasVar' 'Det   = 'False
  HasVar' 'Indet = 'False
  HasVar' _      = 'True

type HasVar l = HasVar' l ~ 'True

type family HasApp' (l :: Level) where
  HasApp' 'Indet = 'False
  HasApp' _      = 'True

type HasApp l = HasApp' l ~ 'True

type family HasLam' (l :: Level) where
  HasLam' 'Term  = 'True
  HasLam' 'Indet = 'True
  HasLam' _      = 'False

type HasLam l = HasLam' l ~ 'True

type family HasElim' (l :: Level) where
  HasElim' 'Term  = 'True
  HasElim' 'Indet = 'True
  HasElim' _      = 'False

type HasElim l = HasElim' l ~ 'True

type family HasFix' (l :: Level) where
  HasFix' 'Term = 'True
  HasFix' 'Det  = 'True
  HasFix' _     = 'False

type HasFix l = HasFix' l ~ 'True

type family HasLet' (l :: Level) where
  HasLet' 'Term = 'True
  HasLet' _     = 'False

type HasLet l = HasLet' l ~ 'True

type HasArr l = (HasCtr l, HasApp l)

type NoBind l =
  ( HasLam' l ~ 'False
  , HasElim' l ~ 'False
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
data Expr' (r :: Func) (l :: Level) v h where
  Hole :: h -> Expr' r l v h
  Ctr :: HasCtr l => Ctr -> Expr' r l v h
  Var :: HasVar l => v -> Expr' r l v h
  App :: HasApp l => Rec r l v h -> Rec r l v h -> Expr' r l v h
  Lam :: HasLam l => v -> Rec r l v h -> Expr' r l v h
  Let :: HasLet l => v -> Rec r l v h -> Rec r l v h -> Expr' r l v h
  Elim :: HasElim l => [(Ctr, Rec r l v h)] -> Expr' r l v h
  Fix :: HasFix l => Expr' r l v h

data Func' a = Fixed | Base a | Ann a | Opt a
  deriving (Eq, Ord, Show, Read)

type Func = Func' Kind.Type

-- TODO: perhaps we should remove Fix and just have Ann, as it would make
-- things much simpler with generalizing functions, but it would be slightly
-- less efficient. For optional annotations, it might be better to have an
-- actual Ann constructor in Expr', as well as some functions converting from
-- `Ann a l v h -> Expr' ('Annotate l a) v h` and
-- `Expr' ('Annotate l a) v h -> Ann (Maybe a) l v h`.
type family Rec (f :: Func) (l :: Level) v h where
  Rec 'Fixed l v h = Expr l v h
  Rec ('Base c) _ _ _ = c
  Rec ('Ann a) l v h = Ann a l v h

type Expr = Expr' 'Fixed
type Base a = Expr' ('Base a)
type Ann a l v h = Annot (Expr' ('Ann a) l v h) a

deriving instance (Eq v, Eq h, Eq (Rec r l v h)) => Eq (Expr' r l v h)
deriving instance (Ord v, Ord h, Ord (Rec r l v h)) => Ord (Expr' r l v h)
deriving instance (Show v, Show h, Show (Rec r l v h)) => Show (Expr' r l v h)

pattern Arr :: () => HasArr l => Expr l v h -> Expr l v h -> Expr l v h
pattern Arr t u = App (App (Ctr (MkCtr "->")) t) u

pattern Case :: () => (HasApp l, HasElim l) =>
  Expr l v h -> [(Ctr, Expr l v h)] -> Expr l v h
pattern Case x xs = App (Elim xs) x

type Type  = Expr 'Type Var Void
type Term  = Expr 'Term
type Value = Expr 'Value Void

-- | Indetermine expressions are either a (pattern) lambda followed by a term,
-- or a hole.
type Indet = Base (Term Var Hole) 'Indet Var Hole

-- | Results are determinate expressions whose holes are indeterminate
-- expressions capturing the local scope.
type Result = Expr 'Det Var (Annot Indet Scope)

newtype Scope = Scope (Map Var Result)
  deriving newtype (Eq, Ord, Show)

-- Morphisms {{{

rec :: Traversal (Expr' r l v h) (Expr' r' l v h) (Rec r l v h) (Rec r' l v h)
rec go = \case
  Hole h -> pure $ Hole h
  Ctr c -> pure $ Ctr c
  Var v -> pure $ Var v
  App f x -> App <$> go f <*> go x
  Lam a x -> Lam a <$> go x
  Let a x y -> Let a <$> go x <*> go y
  Elim xs -> Elim <$> traverse (traverse go) xs
  Fix -> pure Fix

paraExprM :: Monad m => (Expr l v h -> Base c l v h -> m c) -> Expr l v h -> m c
paraExprM g e = g e =<< forOf rec e (paraExprM g)

cataExprM :: Monad m => (Base c l v h -> m c) -> Expr l v h -> m c
cataExprM = paraExprM . const

paraExpr :: (Expr l v h -> Base c l v h -> c) -> Expr l v h -> c
paraExpr g e = g e (over rec (paraExpr g) e)

cataExpr :: (Base c l v h -> c) -> Expr l v h -> c
cataExpr = paraExpr . const

apoExprM :: Monad m =>
  (c -> m (Either (Expr l v h) (Base c l v h))) -> c -> m (Expr l v h)
apoExprM g e = g e >>= either return \x -> forOf rec x (apoExprM g)

anaExprM :: Monad m => (c -> m (Base c l v h)) -> c -> m (Expr l v h)
anaExprM = apoExprM . (fmap return .)

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
  , f ~ HasElim l, f' ~ HasElim l', f => f'
  , g ~ HasFix l, g' ~ HasFix l', g => g'
  ) => Expr l v h -> Expr l' v h
coerceExpr = cataExpr \case
  Hole h -> Hole h
  Ctr c -> Ctr c
  Var v -> Var v
  App f x -> App f x
  Lam a x -> Lam a x
  Let a x y -> Let a x y
  Elim xs -> Elim xs
  Fix -> Fix

fixExprM :: Monad m => Base (Expr l v h) l v h -> m (Expr l v h)
fixExprM = flip (forOf rec) return

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
  Elim xs -> Elim <$> traverse sequenceA xs
  Fix -> pure Fix

holes :: Traversal (Expr l v h) (Expr l v h') h h'
holes = holes' . fmap (fmap Hole)

free' :: NoBind l => Traversal (Expr l v h) (Expr l v' h) v (Expr l v' h)
free' g = cataExpr \case
  Hole h -> pure $ Hole h
  Ctr c -> pure $ Ctr c
  Var v -> g v
  App f x -> App <$> f <*> x
  Fix -> pure Fix

free :: (NoBind l, HasVar l) => Traversal (Expr l v h) (Expr l v' h) v v'
free = free' . fmap (fmap Var)

-- }}}

-- Smart constructors {{{

-- TODO: replace with more general infix function
arrs :: (Foldable f, HasArr l) => f (Expr l v h) -> Expr l v h
arrs = foldr1 Arr

unArrs :: Expr l v h -> NonEmpty (Expr l v h)
unArrs = \case
  Arr t u -> t `cons` unArrs u
  t -> pure t

{-# COMPLETE Arrs #-}
pattern Arrs :: Expr l v h -> [Expr l v h] -> Expr l v h
pattern Arrs a bs <- (unArrs -> (a :| bs))

{-# COMPLETE Args #-}
pattern Args :: [Expr l v h] -> Expr l v h -> Expr l v h
pattern Args as b <- (unsnoc . unArrs -> (as, b))

apps :: (Foldable f, HasApp l) => Expr l v h -> f (Expr l v h) -> Expr l v h
apps = foldl App

unApps :: Expr l v h -> NonEmpty (Expr l v h)
unApps = reverse . go where
  go :: Expr l v h -> NonEmpty (Expr l v h)
  go = \case
    App f x -> x `cons` go f
    e -> pure e

{-# COMPLETE Apps #-}
pattern Apps :: Expr l v h -> [Expr l v h] -> Expr l v h
pattern Apps f xs <- (unApps -> (f :| xs))

lams :: (Foldable f, HasLam l) => f v -> Expr l v h -> Expr l v h
lams = flip (foldr Lam)

unLams :: Expr l v h -> ([v], Expr l v h)
unLams = \case
  Lam a x -> first (a:) $ unLams x
  e -> ([], e)

{-# COMPLETE Lams #-}
pattern Lams :: [v] -> Expr l v h -> Expr l v h
pattern Lams as x <- (unLams -> (as, x))

-- }}}

-- Polytypes {{{

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

-- TODO: instantiation should also be able to introduce new type variables,
-- e.g. by instantiating `id` to `forall a. List a -> List a`. Perhaps we
-- should just substitute the monotype and recompute free quantified variables.
instantiate :: Map Var Type -> Poly -> Poly
instantiate th (Poly fr ty) =
  Poly (filter (`notElem` Map.keys th) fr) (subst th ty)

refresh :: FreshFree m => Poly -> m Poly
refresh (Poly as t) = do
  th <- for as \a -> (a,) . freeId <$> fresh
  let u = subst (Var <$> Map.fromList th) t
  return $ Poly (snd <$> th) u

-- | Instantiate all quantified variables of a polytype with fresh variables.
instantiateFresh :: FreshFree m => Poly -> m Type
instantiateFresh p = do
  Poly _ t <- refresh p
  return t

-- }}}

-- TODO: maybe move these definitions somewhere else

data Annot x a = Annot x a
  deriving (Eq, Ord, Show)

ann :: Lens' (Annot x a) a
ann = lens (\(Annot _ a) -> a) \(Annot x _) a -> Annot x a

newtype Unit = Unit ()
  deriving newtype (Eq, Ord, Show, Read, Semigroup, Monoid)

data HoleCtx = HoleCtx Type (Map Var VarId)
  deriving (Eq, Ord, Show)

goal :: Lens' HoleCtx Type
goal = lens (\(HoleCtx t _) -> t) \(HoleCtx _ vs) t -> HoleCtx t vs

local :: Lens' HoleCtx (Map Var VarId)
local = lens (\(HoleCtx _ vs) -> vs) \(HoleCtx t _) vs -> HoleCtx t vs

class HasCtxs a where
  holeCtxs :: Lens' a (Map Hole HoleCtx)

-- TODO: what else do we need to track for local variables?
-- Variable name, type, number of holes it appears in, number of occurrences
data Variable = Variable Var Type Int Int
  deriving (Eq, Ord, Show)

varType :: Lens' Variable Type
varType = lens (\(Variable _ t _ _) -> t) \(Variable x _ i n) t ->
  Variable x t i n

class HasVars a where
  variables :: Lens' a (Map VarId Variable)

data Sketch = Sketch Var Poly (Term Var Unit)
  deriving (Eq, Ord, Show)

-- Module {{{

data Signature = MkSignature Var Poly
  deriving (Eq, Ord, Show)

data Binding v h = MkBinding Var (Term v h)
  deriving (Eq, Ord, Show)

data Datatype = MkDatatype Ctr [Var] [(Ctr, [Type])]
  deriving (Eq, Ord, Show)

constructors :: Datatype -> [(Ctr, Poly)]
constructors (MkDatatype d as cs) = cs <&> second \ts ->
  Poly as (arrs (ts ++ [apps (Ctr d) (Var <$> as)]))

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

ctrs :: Module a -> [(Ctr, Poly)]
ctrs (Module xs) = xs >>= \case
  Datatype d -> constructors d
  _ -> []

sigs :: Module a -> [(Var, Poly)]
sigs (Module xs) = xs >>= \case
  Signature (MkSignature x t) -> [(x, t)]
  _ -> []

binds :: Module a -> [(Var, Term Var a)]
binds (Module xs) = xs >>= \case
  Binding (MkBinding x t) -> [(x, t)]
  _ -> []

functions :: Module a -> [(Var, (Term Var a, Poly))]
functions m = mapMaybe (\(v, t) -> (v,) . (,t) <$> lookup v (binds m)) $ sigs m

-- }}}

-- Helper functions {{{

-- | Substitute variables in an expression without variables bindings according
-- to a map of variable substitutions.
subst :: (Ord v, expr ~ Expr l v h, NoBind l) => Map v expr -> expr -> expr
subst th = cataExpr \case
  Var v | Just x <- Map.lookup v th -> x
  e -> fixExpr e

-- | Fill holes in an expression according to a map of hole fillings.
fill :: (Ord h, expr ~ Expr l v h) => Map h expr -> expr -> expr
fill th = cataExpr \case
  Hole h | Just x <- Map.lookup h th -> x
  e -> fixExpr e

-- | All subexpressions, including the expression itself.
dissect :: Expr l v h -> [Expr l v h]
dissect = paraExpr \e -> (e:) . view rec

-- | Remove all annotations from an expression.
strip :: Ann a l v h -> Expr l v h
strip = cataAnn (const fixExpr)

-- | Gather all subexpressions along with their annotation.
collect :: Ann a l v h -> [Annot (Expr l v h) a]
collect = paraAnn \a t -> Annot (strip a) (view ann a) : view rec t

-- | Uniquely number all holes in an expression.
number :: (Traversable t, MonadFresh n m) => t a -> m (t (n, a))
number = traverse \x -> (,x) <$> fresh

-- | Eta expand all holes in a sketch.
etaExpand :: (FreshVarId m, MonadState s m, HasCtxs s, HasVars s) =>
  Term Var Hole -> m (Term Var Hole)
etaExpand = cataExprM \case
  Hole h -> do
    ctxs <- use holeCtxs
    case Map.lookup h ctxs of
      Nothing -> return $ Hole h
      Just ctx -> do
        -- Split the type in the arguments and the result type
        let Args ts u = view goal ctx
        -- Couple each argument with a fresh name
        xs <- number ts
        let locals' = Map.fromList ((varId &&& id) . fst <$> xs)
        -- Update the hole context
        modifying holeCtxs $ Map.insert h $ HoleCtx u (view local ctx <> locals')
        let vs = Map.fromList $ xs <&> \(x, t) -> (x, Variable (varId x) t 1 0)
        -- traceShowM vars
        modifying variables (vs <>)
        -- Eta expand the hole
        return $ lams (varId . fst <$> xs) (Hole h)
  e -> fixExprM e

-- }}}

-- Pretty printing {{{

instance Pretty Unit where
  pretty _ = space

instance Pretty Poly where
  pretty = \case
    Poly [] t -> pretty t
    Poly xs t -> "forall" <+> sep (pretty <$> xs) <> dot <+> pretty t

instance (Pretty a, Pretty (Annot a b)) => Pretty (Annot a (Maybe b)) where
  pretty (Annot x a) = maybe (pretty x) (pretty . Annot x) a

instance Pretty a => Pretty (Annot a Scope) where
  pretty (Annot x _) = "[..]" <> pretty x

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
    App f x -> pretty f <+> pretty x
    Lam a x -> parens $ "\\" <> pretty a <+> "->" <+> parens (pretty x)
    Let a x e -> "let" <+> pretty a <+> "=" <+> pretty x <+> "in" <+> pretty e
    Elim xs -> "\\case" <+>
      mconcat (intersperse "; " $ xs <&> \(p, a) ->
        pretty p <+> "->" <+> pretty a)
    Fix -> "fix"

prettyParen :: Bool -> Doc ann -> Doc ann
prettyParen b t
  | b = parens t
  | otherwise = t

instance (Pretty a, Pretty v, Pretty h) => Pretty (Base a l v h) where
  pretty = \case
    Hole h -> braces $ pretty h
    Var x -> pretty x
    Ctr c -> pretty c
    App f x -> sep [pretty f, pretty x]
    Lam a x -> "\\" <> pretty a <+> "->" <+> pretty x
    Let a x y -> "let" <+> pretty a <+> "=" <+> pretty x <+> "in" <+> pretty y
    Elim xs -> "\\case" <+> mconcat (intersperse "; " $ xs <&> \(p, a) ->
      pretty p <+> "->" <+> pretty a)
    Fix -> "fix"

pp :: (Pretty v, Pretty h) => Int -> Expr l v h -> Doc ann
pp i = \case
  Hole h -> braces $ pretty h
  Var x -> pretty x
  Ctr c -> pretty c
  Arr t u -> prettyParen (i > 1) $ sep [pp 2 t, "->", pp 1 u]
  Case x xs -> prettyParen (i > 0) $ "case" <+> pp 0 x <+> "of" <+>
    mconcat (intersperse "; " $ xs <&> \(p, Lams as b) ->
      sep (pretty p : fmap pretty as) <+> "->" <+> pp 0 b)
  App f x -> prettyParen (i > 2) $ sep [pp 2 f, pp 3 x]
  Lam a (Lams as x) -> prettyParen (i > 0) $
    "\\" <> sep (pretty <$> a:as) <+> "->" <+> pp 0 x
  Let a (Lams as x) e -> prettyParen (i > 0) $
    "let" <+> pretty a <+> sep (pretty <$> as)
    <+> "=" <+> pp 0 x <+> "in" <+> pp 0 e
  Elim xs -> prettyParen (i > 0) $ "\\case" <+>
    mconcat (intersperse "; " $ xs <&> \(p, a) ->
      pretty p <+> "->" <+> pp 0 a)
  Fix -> "fix"

instance (Pretty v, Pretty h) => Pretty (Expr l v h) where
  pretty = pp 0

instance Pretty Datatype where
  pretty (MkDatatype d as cs) =
    "data" <+> sep (pretty d : fmap pretty as) <+>
      ( align . sep . zipWith (<+>) ("=" : repeat "|")
      $ cs <&> \(c, xs) -> pretty (apps (Ctr c) xs)
      )

instance Pretty Signature where
  pretty (MkSignature x t) = pretty x <+> "::" <+> pretty t

instance (Pretty v, Pretty h) => Pretty (Binding v h) where
  pretty (MkBinding x (Lams as e)) =
    sep (pretty x : fmap pretty as) <+> "=" <+> pretty e

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
