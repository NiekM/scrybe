{-# LANGUAGE RankNTypes, PolyKinds, UndecidableInstances #-}

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
import Prettyprinter hiding (list)
import qualified Prettyprinter as P
import qualified RIO.Map as Map

-- Levels {{{

type family May (c :: Kind.Type -> Kind.Constraint) a :: Kind.Constraint where
  May c 'Nothing = ()
  May c ('Just a) = c a

type family AllMay (c :: Kind.Type -> Kind.Constraint) a :: Kind.Constraint where
  AllMay c '[]       = ()
  AllMay c (x ': xs) = (May c x, AllMay c xs)

type family IsJust x where
  IsJust 'Nothing  = 'False
  IsJust ('Just _) = 'True

-- TODO: do we need a Core language and a surface language?
-- TODO: maybe add kinds?
-- TODO: maybe level should contain values (concrete evaluation results) as
-- well as 'results' (as seen in Smyth)
data Level' a
  = Type    -- ^ Type level expressions
  | Term a  -- ^ Term level expressions
  | Lambda  -- ^ Lambda expressions
  | Det     -- ^ Determinate results
  | Ind     -- ^ Indeterminate results
  | Value   -- ^ Concrete values
  | Example -- ^ Input-output examples
  deriving (Eq, Ord, Show, Read)

type Level = Level' Kind.Type

type family Hole' (l :: Level) where
  Hole' ('Term t) = 'Just t
  Hole' 'Ind      = 'Just Hole
  Hole' 'Det      = 'Just (Annot Indet Scope)
  Hole' 'Example  = 'Just Unit
  Hole' _         = 'Nothing

type HasHole l h = Hole' l ~ 'Just h

-- TODO: perhaps each of Ctr, Elim and Prj should have their own constructor
-- type, which should usually match, but does not have to. Similarly for
-- variables.
type family Ctr' (l :: Level) where
  Ctr' 'Lambda = 'Nothing
  Ctr' _       = ('Just Ctr)

type family HasCtr' (l :: Level) where
  HasCtr' 'Ind = 'False
  HasCtr' l    = IsJust (Ctr' l)

type HasCtr l c = (HasCtr' l ~ 'True, Ctr' l ~ 'Just c)

type family Var' (l :: Level) where
  Var' 'Det     = 'Nothing
  Var' 'Value   = 'Nothing
  Var' 'Example = ('Just Value)
  Var' _        = ('Just Var)

type family HasVar' (l :: Level) where
  HasVar' 'Example = 'False
  HasVar' l        = IsJust (Var' l)

type HasVar l v = (HasVar' l ~ 'True, Var' l ~ 'Just v)

type family HasApp' (l :: Level) where
  HasApp' 'Ind = 'False
  HasApp' _    = 'True

type HasApp l = HasApp' l ~ 'True

type family HasLam' (l :: Level) where
  HasLam' ('Term _) = 'True
  HasLam' 'Ind      = 'True
  HasLam' 'Example  = 'True
  HasLam' _         = 'False

type HasLam l v = (HasLam' l ~ 'True, Var' l ~ 'Just v)

type family HasElim' (l :: Level) where
  HasElim' ('Term _) = 'True
  HasElim' 'Ind      = 'True
  HasElim' _         = 'False

type HasElim l c = (HasElim' l ~ 'True, Ctr' l ~ 'Just c)

type family HasFix' (l :: Level) where
  HasFix' ('Term _) = 'True
  HasFix' 'Det      = 'True
  HasFix' _         = 'False

type HasFix l = HasFix' l ~ 'True

type family HasPrj' (l :: Level) where
  HasPrj' 'Det = 'True
  HasPrj' _    = 'False

type HasPrj l c = (HasPrj' l ~ 'True, Ctr' l ~ 'Just c)

type family HasLet' (l :: Level) where
  HasLet' ('Term _) = 'True
  HasLet' _         = 'False

type HasLet l v = (HasLet' l ~ 'True, Var' l ~ 'Just v)

type HasArr l = (HasCtr l Ctr, HasApp l)

type NoBind l = (HasLam' l ~ 'False, HasLet' l ~ 'False)

-- }}}

-- | The type of expressions, generic in
--
-- r: the type of recursion, used to differentiate between base functors and
-- their fixed point, as well as allowing recursive calls to be interleaved
-- with annotations.
--
-- l: the level of the expression, containing both universe levels as well as
-- different compiler stages. This parameter is used to determine which
-- constructors are available, as well as the types of holes, constructors and
-- variables in the expression.
--
data Expr' (r :: Func) (l :: Level) where
  Hole :: HasHole l h => h -> Expr' r l
  Ctr :: HasCtr l c => c -> Expr' r l
  Var :: HasVar l v => v -> Expr' r l
  App :: HasApp l => Rec r l -> Rec r l -> Expr' r l
  Lam :: HasLam l v => v -> Rec r l -> Expr' r l
  Let :: HasLet l v => v -> Rec r l -> Rec r l -> Expr' r l
  Elim :: HasElim l c => [(c, Rec r l)] -> Expr' r l
  Fix :: HasFix l => Expr' r l
  Prj :: HasPrj l c => c -> Int -> Expr' r l

data Func' a = Fixed | Base a | Ann a
  deriving (Eq, Ord, Show, Read)

type Func = Func' Kind.Type

-- TODO: perhaps we should remove Fix and just have Ann, as it would make
-- things much simpler with generalizing functions, but it would be slightly
-- less efficient. For optional annotations, it might be better to have an
-- actual Ann constructor in Expr', as well as some functions converting from
-- `Ann a l v h -> Expr' ('Annotate l a) v h` and
-- `Expr' ('Annotate l a) v h -> Ann (Maybe a) l v h`.
type family Rec (f :: Func) (l :: Level) where
  Rec 'Fixed l = Expr l
  Rec ('Base c) _ = c
  Rec ('Ann a) l = Ann a l

type Expr = Expr' 'Fixed
type Base a = Expr' ('Base a)
type Ann a l = Annot (Expr' ('Ann a) l) a

deriving instance
  ( h ~ Hole' l, May Eq h
  , c ~ Ctr' l, May Eq c
  , v ~ Var' l, May Eq v
  , Eq (Rec r l)) => Eq (Expr' r l)
deriving instance
  ( h ~ Hole' l, May Eq h, May Ord h
  , c ~ Ctr' l, May Eq c, May Ord c
  , v ~ Var' l, May Eq v, May Ord v
  , Ord (Rec r l)) => Ord (Expr' r l)
deriving instance
  ( h ~ Hole' l, May Show h
  , c ~ Ctr' l, May Show c
  , v ~ Var' l, May Show v
  , Show (Rec r l)) => Show (Expr' r l)

pattern Arr :: HasCtr l Ctr => HasArr l => Expr l -> Expr l -> Expr l
pattern Arr t u = App (App (Ctr (MkCtr "->")) t) u

pattern Case :: () => (HasApp l, HasElim l c) =>
  Expr l -> [(c, Expr l)] -> Expr l
pattern Case x xs = App (Elim xs) x

type Type    = Expr 'Type
type Term h  = Expr ('Term h)
type Value   = Expr 'Value
type Example = Expr 'Example

-- | Indetermine expressions are either a (pattern) lambda followed by a term,
-- or a hole.
type Indet = Base (Term Hole) 'Ind

-- | Results are determinate expressions whose holes are indeterminate
-- expressions capturing the local scope.
type Result = Expr 'Det

newtype Scope = Scope (Map Var Result)
  deriving newtype (Eq, Ord, Show)

-- Morphisms {{{

rec :: Traversal (Expr' r l) (Expr' r' l) (Rec r l) (Rec r' l)
rec go = \case
  Hole h -> pure $ Hole h
  Ctr c -> pure $ Ctr c
  Var v -> pure $ Var v
  App f x -> App <$> go f <*> go x
  Lam a x -> Lam a <$> go x
  Let a x y -> Let a <$> go x <*> go y
  Elim xs -> Elim <$> traverse (traverse go) xs
  Fix -> pure Fix
  Prj c i -> pure $ Prj c i

paraExprM :: Monad m => (Expr l -> Base c l -> m c) -> Expr l -> m c
paraExprM g e = g e =<< forOf rec e (paraExprM g)

cataExprM :: Monad m => (Base c l -> m c) -> Expr l -> m c
cataExprM = paraExprM . const

paraExpr :: (Expr l -> Base c l -> c) -> Expr l -> c
paraExpr g e = g e (over rec (paraExpr g) e)

cataExpr :: (Base c l -> c) -> Expr l -> c
cataExpr = paraExpr . const

apoExprM :: Monad m => (c -> m (Either (Expr l) (Base c l))) -> c -> m (Expr l)
apoExprM g e = g e >>= either return \x -> forOf rec x (apoExprM g)

anaExprM :: Monad m => (c -> m (Base c l)) -> c -> m (Expr l)
anaExprM = apoExprM . (fmap return .)

apoExpr :: (c -> Either (Expr l) (Base c l)) -> c -> Expr l
apoExpr g e = either id (over rec (apoExpr g)) (g e)

anaExpr :: (c -> Base c l) -> c -> Expr l
anaExpr = apoExpr . (return .)

fixExprM :: Monad m => Base (Expr l) l -> m (Expr l)
fixExprM = flip (forOf rec) return

fixExpr :: Base (Expr l) l -> Expr l
fixExpr = over rec id

mapAnn :: (a -> b) -> Ann a l -> Ann b l
mapAnn f (Annot e a) = Annot (over rec (mapAnn f) e) (f a)

paraAnn :: (Ann a l -> Base c l -> c) -> Ann a l -> c
paraAnn g (Annot e a) = g (Annot e a) (over rec (paraAnn g) e)

cataAnn :: (a -> Base c l -> c) -> Ann a l -> c
cataAnn = paraAnn . (. view ann)

-- }}}

-- Lenses {{{

holes' :: Traversal (Expr ('Term h)) (Expr ('Term h')) h (Expr ('Term h'))
holes' g = cataExpr \case
  Hole h -> g h
  Ctr c -> pure $ Ctr c
  Var v -> pure $ Var v
  App f x -> App <$> f <*> x
  Lam a x -> Lam a <$> x
  Let a x y -> Let a <$> x <*> y
  Elim xs -> Elim <$> traverse sequenceA xs
  Fix -> pure Fix

holes :: Traversal (Expr ('Term h)) (Expr ('Term h')) h h'
holes = holes' . fmap (fmap Hole)

free :: Traversal' Type Var
free g = cataExpr \case
  Ctr c -> pure $ Ctr c
  Var v -> Var <$> g v
  App f x -> App <$> f <*> x

-- }}}

-- Smart constructors {{{

arrs :: (Foldable f, HasArr l) => f (Expr l) -> Expr l
arrs = foldr1 Arr

unArrs :: HasCtr l Ctr => Expr l -> NonEmpty (Expr l)
unArrs = \case
  Arr t u -> t `cons` unArrs u
  t -> pure t

{-# COMPLETE Arrs #-}
pattern Arrs :: HasCtr l Ctr => Expr l -> [Expr l] -> Expr l
pattern Arrs a bs <- (unArrs -> (a :| bs))

{-# COMPLETE Args #-}
pattern Args :: HasCtr l Ctr => [Expr l] -> Expr l -> Expr l
pattern Args as b <- (unsnoc . unArrs -> (as, b))

apps :: (Foldable f, HasApp l) => Expr l -> f (Expr l) -> Expr l
apps = foldl App

unApps :: Expr l -> NonEmpty (Expr l)
unApps = reverse . go where
  go :: Expr l -> NonEmpty (Expr l)
  go = \case
    App f x -> x `cons` go f
    e -> pure e

{-# COMPLETE Apps #-}
pattern Apps :: Expr l -> [Expr l] -> Expr l
pattern Apps f xs <- (unApps -> (f :| xs))

lams :: (Foldable f, HasLam l v) => f v -> Expr l -> Expr l
lams = flip (foldr Lam)

unLams :: HasLam l v => Expr l -> ([v], Expr l)
unLams = \case
  Lam a x -> first (a:) $ unLams x
  e -> ([], e)

{-# COMPLETE Lams #-}
pattern Lams :: HasLam l v => [v] -> Expr l -> Expr l
pattern Lams as x <- (unLams -> (as, x))

nat :: (HasCtr l Ctr, HasApp l) => Int -> Expr l
nat 0 = Ctr "Zero"
nat n = App (Ctr "Succ") (nat $ n - 1)

unNat :: HasCtr l Ctr => Expr l -> Maybe Int
unNat = \case
  Ctr "Zero" -> Just 0
  App (Ctr "Succ") n -> (1+) <$> unNat n
  _ -> Nothing

pattern Nat :: HasCtr l Ctr => Int -> Expr l
pattern Nat n <- (unNat -> Just n)

list :: (HasCtr l Ctr, HasApp l) => [Expr l] -> Expr l
list = foldr (\x y -> apps (Ctr "Cons") [x, y]) (Ctr "Nil")

unList :: HasCtr l Ctr => Expr l -> Maybe [Expr l]
unList = \case
  Ctr "Nil" -> Just []
  Apps (Ctr "Cons") [x, xs] -> (x:) <$> unList xs
  _ -> Nothing

pattern List :: HasCtr l Ctr => [Expr l] -> Expr l
pattern List xs <- (unList -> Just xs)

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

data Sketch = Sketch Var Poly (Term Unit)
  deriving (Eq, Ord, Show)

-- Module {{{

-- TODO: move this to its own file

data Signature = MkSignature Var Poly
  deriving (Eq, Ord, Show)

data Binding h = MkBinding Var (Term h)
  deriving (Eq, Ord, Show)

data Datatype = MkDatatype Ctr [Var] [(Ctr, [Type])]
  deriving (Eq, Ord, Show)

constructors :: Datatype -> [(Ctr, Poly)]
constructors (MkDatatype d as cs) = cs <&> second \ts ->
  Poly as (arrs (ts ++ [apps (Ctr d) (Var <$> as)]))

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

ctrs :: Module a -> [(Ctr, Poly)]
ctrs (Module xs) = xs >>= \case
  Datatype d -> constructors d
  _ -> []

sigs :: Module a -> [(Var, Poly)]
sigs (Module xs) = xs >>= \case
  Signature (MkSignature x t) -> [(x, t)]
  _ -> []

binds :: Module a -> [(Var, Term a)]
binds (Module xs) = xs >>= \case
  Binding (MkBinding x t) -> [(x, t)]
  _ -> []

functions :: Module a -> [(Var, (Term a, Poly))]
functions m = mapMaybe (\(v, t) -> (v,) . (,t) <$> lookup v (binds m)) $ sigs m

arities :: Module a -> Map Ctr Int
arities m = Map.fromList (ctrs m) <&> \(Poly _ (Args as _)) -> length as

-- }}}

-- Helper functions {{{

-- | Substitute variables in an expression without variables bindings according
-- to a map of variable substitutions.
subst :: (HasVar l v, Ord v, NoBind l) => Map v (Expr l) -> Expr l -> Expr l
subst th = cataExpr \case
  Var v | Just x <- Map.lookup v th -> x
  e -> fixExpr e

-- | Fill holes in an expression according to a map of hole fillings.
fill :: (HasHole l h, Ord h) => Map h (Expr l) -> Expr l -> Expr l
fill th = cataExpr \case
  Hole h | Just x <- Map.lookup h th -> x
  e -> fixExpr e

-- | All subexpressions, including the expression itself.
dissect :: Expr l -> [Expr l]
dissect = paraExpr \e -> (e:) . view rec

-- | Remove all annotations from an expression.
strip :: Ann a l -> Expr l
strip = cataAnn (const fixExpr)

-- | Gather all subexpressions along with their annotation.
collect :: Ann a l -> [Annot (Expr l) a]
collect = paraAnn \a t -> Annot (strip a) (view ann a) : view rec t

-- | Uniquely number all holes in an expression.
number :: (Traversable t, MonadFresh n m) => t a -> m (t (n, a))
number = traverse \x -> (,x) <$> fresh

-- | Eta expand all holes in a sketch.
etaExpand :: (FreshVarId m, MonadState s m, HasCtxs s, HasVars s) =>
  Term Hole -> m (Term Hole)
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
        modifying holeCtxs $
          Map.insert h $ HoleCtx u (view local ctx <> locals')
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

prettyParen :: Bool -> Doc ann -> Doc ann
prettyParen b t
  | b = parens t
  | otherwise = t

pExpr :: AllMay Pretty '[Hole' l, Var' l, Ctr' l] =>
  (Int -> Rec r l -> Doc ann) -> Int -> Expr' r l -> Doc ann
pExpr p i = \case
  Hole h -> braces $ pretty h
  Var x -> pretty x
  Ctr c -> pretty c
  App f x -> prettyParen (i > 2) $ sep [p 2 f, p 3 x]
  Lam a x -> prettyParen (i > 0) $ "\\" <> pretty a <+> "->" <+> p 0 x
  Let a x e -> prettyParen (i > 0) $ "let" <+> pretty a <+> "=" <+> p 0 x
    <+> "in" <+> p 0 e
  Elim xs -> prettyParen (i > 0) $ "\\case" <+> mconcat
    (intersperse "; " $ xs <&> \(c, b) -> pretty c <+> "->" <+> p 0 b)
  Fix -> "fix"
  Prj c n -> pretty c <> dot <> pretty n

-- Syntax sugar {{{

-- TODO: can we do something similar for parsing?

type Sugar l ann =
  (Int -> Expr l -> Doc ann) -> Int -> Expr l -> Maybe (Doc ann)

none :: Sugar l ann
none _ _ _ = Nothing

orTry :: Sugar l ann -> Sugar l ann -> Sugar l ann
orTry x y p i e = maybe (y p i e) return $ x p i e

sTerm :: (HasLam l v, Pretty v, May Pretty (Ctr' l)) => Sugar l ann
sTerm p i = \case
  Lam a (Lams as x) -> Just . prettyParen (i > 0) $
    "\\" <> sep (pretty <$> a:as) <+> "->" <+> p 0 x
  Let a (Lams as x) e -> Just . prettyParen (i > 0) $
    "let" <+> pretty a <+> sep (pretty <$> as)
    <+> "=" <+> p 0 x <+> "in" <+> p 0 e
  Case x xs -> Just . prettyParen (i > 0) $ "case" <+> p 0 x <+> "of" <+>
    mconcat (intersperse "; " $ xs <&> \(c, Lams as b) ->
      sep (pretty c : fmap pretty as) <+> "->" <+> p 0 b)
  _ -> Nothing

sArr :: HasCtr l Ctr => Sugar l ann
sArr p i = \case
  Arr t u -> Just . prettyParen (i > 1) $ sep [p 2 t, "->", p 1 u]
  _ -> Nothing

sLit :: HasCtr l Ctr => Sugar l ann
sLit p _ = \case
  Nat n -> Just $ pretty n
  List xs -> Just $ P.list (p 0 <$> xs)
  _ -> Nothing

sExample :: Sugar 'Example ann
sExample p i = \case
  Lam v (Lams vs x) -> Just . prettyParen (i > 0) $
    sep (withSugarPrec sLit 3 <$> v:vs) <+> "->" <+> p 0 x
  _ -> Nothing

withSugarPrec :: AllMay Pretty '[Hole' l, Var' l, Ctr' l] =>
  Sugar l ann -> Int -> Expr l -> Doc ann
withSugarPrec s n t = fromMaybe (pExpr go n t) (s go n t)
  where go = withSugarPrec s

withSugar :: AllMay Pretty '[Hole' l, Var' l, Ctr' l] =>
  Sugar l ann -> Expr l -> Doc ann
withSugar s = withSugarPrec s 0

-- }}}

instance Pretty h => Pretty (Term h) where
  pretty = withSugar (sLit `orTry` sTerm)

instance Pretty Type where
  pretty = withSugar sArr

instance Pretty Value where
  pretty = withSugar sLit

instance Pretty Example where
  pretty = withSugar sExample

instance Pretty Result where
  pretty = withSugar sLit

instance (Pretty (Ann a ('Term h)), Pretty h)
  => Pretty (Expr' ('Ann a) ('Term h)) where
  pretty = pExpr (const pretty) 0

instance (Pretty a, AllMay Pretty '[Var' l, Hole' l, Ctr' l])
  => Pretty (Base a l) where
  pretty = pExpr (const pretty) 0

instance Pretty Datatype where
  pretty (MkDatatype d as cs) =
    "data" <+> sep (pretty d : fmap pretty as) <+>
      ( align . sep . zipWith (<+>) ("=" : repeat "|")
      $ cs <&> \(c, xs) -> pretty (apps (Ctr c) xs)
      )

instance Pretty Signature where
  pretty (MkSignature x t) = pretty x <+> "::" <+> pretty t

instance Pretty h => Pretty (Binding h) where
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
