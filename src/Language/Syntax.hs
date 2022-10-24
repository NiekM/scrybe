{-# LANGUAGE RankNTypes, PolyKinds, UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}

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
import qualified RIO.List as List
import qualified RIO.NonEmpty as NonEmpty
import Prettyprinter hiding (list)
import Control.Monad.State
import qualified Prettyprinter as P
import qualified RIO.Map as Map
import qualified RIO.Set as Set

-- Levels {{{

type family May (c :: Kind.Type -> Kind.Constraint) a :: Kind.Constraint where
  May c 'Nothing = ()
  May c ('Just a) = c a

type family AllMay (c :: Kind.Type -> Kind.Constraint) a :: Kind.Constraint where
  AllMay c '[]       = ()
  AllMay c (x ': xs) = (May c x, AllMay c xs)

type Consts (c :: Kind.Type -> Kind.Constraint) (l :: Level) =
  AllMay c '[Hole' l, Ctr' l, Var' l, Bind' l]

type family IsJust x where
  IsJust 'Nothing  = 'False
  IsJust ('Just _) = 'True

data Level' a
  = Type    -- ^ Type level expressions
  | Term a  -- ^ Term level expressions
  | Det     -- ^ Determinate results
  | Ind     -- ^ Indeterminate results
  | Value   -- ^ Concrete values
  | Example -- ^ Input-output examples
  deriving (Eq, Ord, Show, Read)

type Level = Level' Kind.Type

class Leveled (l :: Level) where
  type Hole' l :: Maybe Kind.Type
  type Hole' l = 'Nothing

  type Ctr' l :: Maybe Kind.Type
  type Ctr' l = 'Just Ctr

  type Var' l :: Maybe Kind.Type
  type Var' l = 'Just Var

  type Bind' l :: Maybe Kind.Type
  type Bind' l = 'Nothing

  type HasCtr' l :: Bool
  type HasCtr' l = IsJust (Ctr' l)

  type HasApp' l :: Bool
  type HasApp' l = 'True

  type HasLam' l :: Bool
  type HasLam' l = IsJust (Bind' l)

  type HasLet' l :: Bool
  type HasLet' l = 'False

  type HasElim' l :: Bool
  type HasElim' l = 'False

  type HasFix' l :: Bool
  type HasFix' l = 'False

  type HasPrj' l :: Bool
  type HasPrj' l = 'False

type HasHole l h = Hole' l ~ 'Just h
type HasCtr l c = (HasCtr' l ~ 'True, Ctr' l ~ 'Just c)
type HasVar l v = Var' l ~ 'Just v
type HasApp l = HasApp' l ~ 'True
type HasLam l v = (HasLam' l ~ 'True, Bind' l ~ 'Just v)
type HasLet l v = (HasLet' l ~ 'True, Bind' l ~ 'Just v)
type HasElim l c = (HasElim' l ~ 'True, Ctr' l ~ 'Just c)
type HasFix l = HasFix' l ~ 'True
type HasPrj l c = (HasPrj' l ~ 'True, Ctr' l ~ 'Just c)

type HasArr l = (HasCtr l Ctr, HasApp l)
type NoBind l = Bind' l ~ 'Nothing

instance Leveled 'Type where
  type Var' 'Type = 'Just Free

instance Leveled ('Term h) where
  type Hole'    ('Term h) = 'Just h
  type Bind'    ('Term _) = 'Just Var
  type HasLet'  ('Term _) = 'True
  type HasElim' ('Term _) = 'True
  type HasFix'  ('Term _) = 'True

instance Leveled 'Det where
  type Hole'   'Det = 'Just (Annot Scope Indet)
  type Var'    'Det = 'Nothing
  type HasFix' 'Det = 'True
  type HasPrj' 'Det = 'True

instance Leveled 'Ind where
  type Hole'    'Ind = 'Just Hole
  type HasCtr'  'Ind = 'False
  type Bind'    'Ind = 'Just Var
  type Var'     'Ind = 'Nothing
  type HasApp'  'Ind = 'False
  type HasElim' 'Ind = 'True

instance Leveled 'Value where
  type Var' 'Value = 'Nothing

instance Leveled 'Example where
  type Hole' 'Example = 'Just Unit
  type Bind' 'Example = 'Just Value

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
  Ctr  :: HasCtr  l c => c -> Expr' r l
  Var  :: HasVar  l v => v -> Expr' r l
  App  :: HasApp  l   => Rec r l -> Rec r l -> Expr' r l
  Lam  :: HasLam  l v => v -> Rec r l -> Expr' r l
  Let  :: HasLet  l v => v -> Rec r l -> Rec r l -> Expr' r l
  Elim :: HasElim l c => [(c, Rec r l)] -> Expr' r l
  Fix  :: HasFix  l   => Expr' r l
  Prj  :: HasPrj  l c => c -> Int -> Expr' r l

data Func' a = Fixed | Base a | Ann a
  deriving (Eq, Ord, Show, Read)

type Func = Func' Kind.Type

-- TODO: perhaps we should remove Fixed and just have Ann, as it would make
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
type Ann a l = Annot a (Expr' ('Ann a) l)

deriving instance (Consts Eq l, Eq (Rec r l)) => Eq (Expr' r l)
deriving instance (Consts Eq l, Consts Ord l, Ord (Rec r l)) => Ord (Expr' r l)
deriving instance (Consts Show l, Show (Rec r l)) => Show (Expr' r l)

pattern Arr :: HasCtr l Ctr => HasArr l => Expr l -> Expr l -> Expr l
pattern Arr t u = App (App (Ctr "->") t) u

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

type Scope = Map Var Result

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

free :: Traversal' Type Free
free g = cataExpr \case
  Ctr c -> pure $ Ctr c
  Var v -> Var <$> g v
  App f x -> App <$> f <*> x

holesAnn :: Traversal (Ann a ('Term h)) (Ann a ('Term h')) h h'
holesAnn g = cataAnn \t -> fmap (`Annot` t) . \case
  Hole h -> Hole <$> g h
  Ctr c -> pure $ Ctr c
  Var v -> pure $ Var v
  App f x -> App <$> f <*> x
  Lam a x -> Lam a <$> x
  Let a x y -> Let a <$> x <*> y
  Elim xs -> Elim <$> traverse sequenceA xs
  Fix -> pure Fix

-- }}}

-- Smart constructors {{{

arrs :: (Foldable f, HasArr l) => f (Expr l) -> Expr l
arrs = foldr1 Arr

unArrs :: HasCtr l Ctr => Expr l -> NonEmpty (Expr l)
unArrs = \case
  Arr t u -> t `NonEmpty.cons` unArrs u
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
unApps = NonEmpty.reverse . go where
  go :: Expr l -> NonEmpty (Expr l)
  go = \case
    App f x -> x `NonEmpty.cons` go f
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

-- Helper functions {{{

class Subst a where
  subst :: Map Free Type -> a -> a

instance Subst Type where
  subst th = cataExpr \case
    Var v | Just x <- Map.lookup v th -> x
    e -> fixExpr e

instance Subst Poly where
  subst th (Poly as t) =
    Poly as $ subst (Map.withoutKeys th (Set.fromList as)) t

instance (Subst t, Subst a) => Subst (Ann t ('Term a)) where
  subst th = over holesAnn (subst th) . mapAnn (subst th)

-- | Fill holes in an expression according to a map of hole fillings.
fill :: (HasHole l h, Ord h) => Map h (Expr l) -> Expr l -> Expr l
fill th = cataExpr \case
  Hole h | Just x <- Map.lookup h th -> x
  e -> fixExpr e

freeVars :: Term h -> Set Var
freeVars = cataExpr \case
  Lam a x -> Set.delete a x
  Var a -> Set.singleton a
  x -> view rec x

-- | All subexpressions, including the expression itself.
dissect :: Expr l -> [Expr l]
dissect = paraExpr \e -> (e:) . view rec

-- | Remove all annotations from an expression.
strip :: Ann a l -> Expr l
strip = cataAnn (const fixExpr)

-- | Gather all subexpressions along with their annotation.
collect :: Ann a l -> [Annot a (Expr l)]
collect = paraAnn \a t -> Annot (strip a) (view ann a) : view rec t

-- | Uniquely number all holes in an expression.
number :: (Traversable t, MonadFresh n m) => t a -> m (t (n, a))
number = traverse \x -> (,x) <$> fresh

-- | Check if an expression has no holes.
holeFree :: Term a -> Maybe (Term Void)
holeFree = traverseOf holes $ const Nothing

-- Eta expansion {{{

class ExpandHole h m where
  expandHole :: h -> m ([(Var, Type)], h)

instance FreshVar m => ExpandHole Type m where
  expandHole (Args ts t) = (,t) <$> number ts --expandHole t
    -- (xs, u) <- (,t) <$> number ts --expandHole t
    -- return (xs, HoleCtx u (vs <> (Mono <$> Map.fromList xs)))

instance FreshVar m => ExpandHole HoleCtx m where
  expandHole (HoleCtx t vs) = do
    (xs, u) <- expandHole t
    return (xs, HoleCtx u (vs <> (Mono <$> Map.fromList xs)))

etaExpand :: (ExpandHole h' m, FreshVar m) =>
  Lens' h h' -> Term h -> m (Term h)
etaExpand ctx = cataExprM \case
  Hole h -> do
    (xs, ctx') <- expandHole (view ctx h)
    return $ lams (fst <$> xs) (Hole $ set ctx ctx' h)
  e -> fixExprM e

-- }}}

typeSize :: Type -> Int
typeSize = cataExpr \case
  App f a -> 1 + f + a
  _ -> 0

-- }}}

-- Polytypes {{{

data Poly = Poly [Free] Type
  deriving (Eq, Ord, Show)

pattern Mono :: Type -> Poly
pattern Mono t = Poly [] t

-- | Turn a monotype into a polytype by quantifying all its free variables.
poly :: Type -> Poly
poly t = Poly (nubOrd $ toListOf free t) t

alphaEq :: Poly -> Poly -> Maybe (Map Free Free)
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
  Var v | v `elem` as, MkFree c <- v -> Ctr (MkCtr c)
  e -> fixExpr e

freezeUnbound :: Poly -> Poly
freezeUnbound (Poly as t) = Poly as $ flip cataExpr t \case
  Var v | v `notElem` as, MkFree c <- v -> Ctr (MkCtr c)
  e -> fixExpr e

freezeAll :: Type -> Type
freezeAll = cataExpr \case
  Var (MkFree c) -> Ctr (MkCtr c)
  e -> fixExpr e

-- TODO: instantiation should also be able to introduce new type variables,
-- e.g. by instantiating `id` to `forall a. List a -> List a`. Perhaps we
-- should just substitute the monotype and recompute free quantified variables.
instantiate :: Map Free Type -> Poly -> Poly
instantiate th (Poly fr ty) =
  Poly (filter (`notElem` Map.keys th) fr) (subst th ty)

refresh :: FreshFree m => Poly -> m Poly
refresh (Poly as t) = do
  th <- for as \a -> (a,) <$> fresh
  let u = subst (Var <$> Map.fromList th) t
  return $ Poly (snd <$> th) u

-- | Instantiate all quantified variables of a polytype with fresh variables.
instantiateFresh :: FreshFree m => Poly -> m Type
instantiateFresh p = do
  Poly _ t <- refresh p
  return t

-- }}}

-- TODO: maybe move these definitions somewhere else

newtype Annot a x = MkAnnot (a, x)
  deriving newtype (Eq, Ord, Show)
  deriving newtype (Functor, Foldable, Applicative, Monad)

{-# COMPLETE Annot #-}
pattern Annot :: x -> a -> Annot a x
pattern Annot x a = MkAnnot (a, x)

ann :: Lens' (Annot a x) a
ann = lens (\(Annot _ a) -> a) \(Annot x _) a -> Annot x a

newtype Unit = Unit ()
  deriving newtype (Eq, Ord, Show, Read, Semigroup, Monoid)

data HoleCtx = HoleCtx Type (Map Var Poly)
  deriving (Eq, Ord, Show)

goalType :: Lens' HoleCtx Type
goalType = lens (\(HoleCtx t _) -> t) \(HoleCtx _ vs) t -> HoleCtx t vs

localEnv :: Lens' HoleCtx (Map Var Poly)
localEnv = lens (\(HoleCtx _ vs) -> vs) \(HoleCtx t _) vs -> HoleCtx t vs

instance Subst HoleCtx where
  subst th = over goalType (subst th) . over localEnv (subst th <$>)

data Sketch = Sketch Var Poly (Term Unit)
  deriving (Eq, Ord, Show)

-- Defs {{{

-- TODO: move this to its own file

data Signature = MkSignature Var Poly
  deriving (Eq, Ord, Show)

data Binding h = MkBinding Var (Term h)
  deriving (Eq, Ord, Show)

data Datatype = MkDatatype Ctr [Free] [(Ctr, [Type])]
  deriving (Eq, Ord, Show)

data Import = MkImport
  { name :: Text
  , expose :: Maybe [Var]
  } deriving (Eq, Ord, Show)

data Pragma
  = Desc String
  | Include (NonEmpty (Var, Maybe Poly))
  deriving (Eq, Ord, Show)

data Assert = MkAssert (Term Hole) Example
  deriving (Eq, Ord, Show)

data Def a
  = Import Import
  | Signature Signature
  | Binding (Binding a)
  | Datatype Datatype
  | Pragma Pragma
  | Assert Assert
  deriving (Eq, Ord, Show)

newtype Defs a = Defs { getDefs :: [Def a] }
  deriving (Eq, Ord, Show)

imports :: Defs a -> [Import]
imports (Defs ds) = [i | Import i <- ds]

signatures :: Defs a -> [Signature]
signatures (Defs ds) = [s | Signature s <- ds]

bindings :: Defs a -> [Binding a]
bindings (Defs ds) = [b | Binding b <- ds]

datatypes :: Defs a -> [Datatype]
datatypes (Defs ds) = [d | Datatype d <- ds]

pragmas :: Defs a -> [Pragma]
pragmas (Defs ds) = [p | Pragma p <- ds]

asserts :: Defs a -> [Assert]
asserts (Defs ds) = [a | Assert a <- ds]

arity :: Poly -> Int
arity (Poly _ (Args as _)) = length as

type LiveEnv = Map Var Result

data Env = Env
  { _functions    :: Map Var Poly
  , _scope        :: Scope
  , _dataTypes    :: Map Ctr ([Free], [(Ctr, [Type])])
  , _constructors :: Map Ctr Poly
  } deriving (Eq, Ord)

makeLenses 'Env

instance Semigroup Env where
  Env a c e g <> Env b d f h =
    Env (a <> b) (c <> d) (e <> f) (g <> h)

instance Monoid Env where
  mempty = Env mempty mempty mempty mempty

class HasEnv a where
  env :: Lens' a Env

instance HasEnv Env where
  env = id

recBinding :: Binding a -> Binding a
recBinding (MkBinding x e)
  | x `elem` freeVars e = MkBinding x $ App Fix $ Lam x e
  | otherwise = MkBinding x e

recDefs :: Defs a -> Defs a
recDefs (Defs ds) = Defs $ ds <&> \case
  Binding b -> Binding $ recBinding b
  x -> x

relBinds :: Defs Unit -> [Binding Hole]
relBinds (Defs ds) = bs' <&> uncurry MkBinding
  where
    bs = [ (x, e) | Binding (MkBinding x e) <- ds, isNothing (holeFree e)]
    bs' = evalState (forOf (each . _2 . holes) bs $ const fresh) mkFreshState

-- }}}

-- Pretty printing {{{

instance Pretty Unit where
  pretty _ = space

instance Pretty Poly where
  pretty = \case
    Poly [] t -> pretty t
    Poly xs t -> "forall" <+> sep (pretty <$> xs) <> dot <+> pretty t

instance (Pretty b, Pretty (Annot a b)) => Pretty (Annot (Maybe a) b) where
  pretty (Annot x a) = maybe (pretty x) (pretty . Annot x) a

instance Pretty a => Pretty (Annot Scope a) where
  pretty (Annot x s) = pretty s <> pretty x

instance Pretty a => Pretty (Annot Type a) where
  pretty (Annot x t) = pretty x <+> "::" <+> pretty t

instance Pretty a => Pretty (Annot Text a) where
  pretty (Annot x p) = "{-#" <+> pretty p <+> "#-}" <+> pretty x

prettyParen :: Bool -> Doc ann -> Doc ann
prettyParen b t
  | b = parens t
  | otherwise = t

pExpr :: Consts Pretty l =>
  (Int -> Rec r l -> Doc ann) -> Int -> Expr' r l -> Doc ann
pExpr p i = \case
  Hole h -> braces $ pretty h
  Var x -> pretty x
  Ctr c -> pretty c
  App f x -> prettyParen (i > 2) $ sep [p 2 f, p 3 x]
  Lam a x -> prettyParen (i > 0) $ "\\" <> pretty a <+> "->" <+> p 0 x
  Let a x y -> "let" <+> pretty a <+> "=" <+> p 0 x <+> "in" <+> p 0 y
  Elim xs -> prettyParen (i > 0) $ "\\case" <+> mconcat
    (List.intersperse "; " $ xs <&> \(c, b) -> pretty c <+> "->" <+> p 0 b)
  Fix -> "fix"
  Prj c n -> pretty c <> dot <> pretty n

-- Syntax sugar {{{

type Sugar l ann =
  (Int -> Expr l -> Doc ann) -> Int -> Expr l -> Maybe (Doc ann)

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
    mconcat (List.intersperse "; " $ xs <&> \(c, Lams as b) ->
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

sRes :: HasHole l (Annot Scope Indet) => Sugar l ann
sRes _ _ = \case
  Hole (Annot e m) -> Just . parens $ pretty m <+> "|-" <+> pretty e
  _ -> Nothing

sExample :: Sugar 'Example ann
sExample p i = \case
  Lam v (Lams vs x) -> Just . prettyParen (i > 0) $
    "\\" <> sep (withSugarPrec sLit 3 <$> v:vs) <+> "->" <+> p 0 x
  _ -> Nothing

withSugarPrec :: Consts Pretty l => Sugar l ann -> Int -> Expr l -> Doc ann
withSugarPrec s n t = fromMaybe (pExpr go n t) (s go n t)
  where go = withSugarPrec s

withSugar :: Consts Pretty l => Sugar l ann -> Expr l -> Doc ann
withSugar s = withSugarPrec s 0

-- }}}

instance Pretty h => Pretty (Term h) where
  pretty = withSugar (sLit `orTry` sTerm)

instance Pretty Type where
  pretty = withSugar sArr

instance Pretty Value where
  pretty = withSugar sLit

instance Pretty Example where
  pretty = withSugar (sLit `orTry` sExample)

instance Pretty Result where
  pretty = withSugar (sLit `orTry` sRes)

instance (Pretty (Ann a ('Term h)), Pretty h)
  => Pretty (Expr' ('Ann a) ('Term h)) where
  pretty = pExpr (const pretty) 0

instance (Pretty a, Consts Pretty l) => Pretty (Base a l) where
  pretty = pExpr (const pretty) 0

instance Pretty Import where
  pretty (MkImport name Nothing) = "import" <+> pretty name
  pretty (MkImport name (Just exports)) = "import" <+> pretty name <+>
    parens (mconcat $ List.intersperse ", " (pretty <$> exports))

instance Pretty Datatype where
  pretty (MkDatatype d as cs) =
    "data" <+> sep (pretty d : fmap pretty as) <+>
      ( align . sep . zipWith (<+>) ("=" : List.repeat "|")
      $ cs <&> \(c, xs) -> pretty (apps (Ctr c) xs)
      )

instance Pretty Signature where
  pretty (MkSignature x t) = pretty x <+> "::" <+> pretty t

instance Pretty h => Pretty (Binding h) where
  pretty (MkBinding x (Lams as e)) =
    sep (pretty x : fmap pretty as) <+> "=" <+> pretty e

instance Pretty Pragma where
  pretty = fancy . \case
    Desc s -> "DESC" <+> pretty (show s)
    Include xs ->
     let ys = xs <&> \case
           (x, Nothing) -> pretty x
           (x, Just t) -> pretty x <+> "::" <+> pretty t
     in "INCLUDE" <+> fold (NonEmpty.intersperse ", " ys)
    where fancy x = "{-#" <+> x <+> "#-}"

instance Pretty Assert where
  pretty (MkAssert a ex) = "assert" <+> pretty a <+> pretty ex

instance Pretty Sketch where
  pretty (Sketch x s b) = vsep
    [pretty $ MkSignature x s, pretty $ MkBinding x b]

instance Pretty a => Pretty (Def a) where
  pretty = \case
    Import i -> pretty i
    Signature s -> pretty s
    Binding b -> pretty b
    Datatype d -> pretty d
    Pragma p -> pretty p
    Assert a -> pretty a

instance Pretty a => Pretty (Defs a) where
  pretty (Defs cs) = vsep $ fmap pretty cs

-- }}}
