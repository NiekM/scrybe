{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}

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
import qualified Prettyprinter as P
import qualified RIO.Map as Map
import qualified RIO.Set as Set

-- Identifiers {{{

newtype Hole = MkHole Natural
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (Num, Pretty, NFData, Count)

newtype Free = MkFree Text
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (IsString, Pretty, NFData)
  deriving Count via TextVar "t"

-- NOTE: we assume that variables are always lowercase and constructors are
-- always uppercase.

newtype Var = MkVar Text
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (IsString, Pretty, NFData)
  deriving Count via TextVar "a"

newtype Ctr = MkCtr Text
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (IsString, Pretty, NFData)

-- }}}

-- Levels {{{

type Consts (c :: Kind.Type -> Kind.Constraint) (l :: Level) =
  (May c (Hole' l), May c (Var' l))

data Level' a
  = Type    -- ^ Type level expressions
  | Term a  -- ^ Term level expressions
  | Det     -- ^ Determinate results
  | Ind     -- ^ Indeterminate results
  | Value   -- ^ Concrete values
  | Example -- ^ Input-output examples
  deriving (Eq, Ord, Show, Read)

type Level = Level' Kind.Type

data EXPR = HOLE | CTR | VAR | APP | LAM | LET | ELIM | FIX | PRJ

class IsExpr (l :: Level) where
  type Hole' l :: Maybe Kind.Type
  type Hole' l = 'Nothing
  type Var'  l :: Maybe Kind.Type
  type Var'  l = 'Nothing
  type Ctrs' l :: [EXPR]

type HAS (c :: EXPR) (l :: Level) = Elem c (Ctrs' l)

type HasHole l h = Hole' l ~ 'Just h
type HasCtr  l   = HAS 'CTR l
type HasApp  l   = HAS 'APP l
type HasElim l   = HAS 'ELIM l
type HasFix  l   = HAS 'FIX l
type HasPrj  l   = HAS 'PRJ l
type HasVar  l v = (HAS 'VAR l, Var' l ~ 'Just v)
type HasLam  l v = (HAS 'LAM l, Var' l ~ 'Just v)
type HasLet  l v = (HAS 'LET l, Var' l ~ 'Just v)

type HasArr l = (HasCtr l, HasApp l)

instance IsExpr 'Type where
  type Var'  'Type = 'Just Free
  type Ctrs' 'Type = ['VAR, 'CTR, 'APP]

instance IsExpr ('Term h) where
  type Hole' ('Term h) = 'Just h
  type Var'  ('Term h) = 'Just Var
  type Ctrs' ('Term h) = ['VAR, 'CTR, 'APP, 'LAM, 'LET, 'ELIM, 'FIX]

instance IsExpr 'Det where
  type Hole' 'Det = 'Just (Scope, Indet)
  type Ctrs' 'Det = ['CTR, 'APP, 'FIX, 'PRJ]

instance IsExpr 'Ind where
  type Hole' 'Ind = 'Just Hole
  type Var'  'Ind = 'Just Var
  type Ctrs' 'Ind = ['LAM, 'ELIM]

instance IsExpr 'Value where
  type Ctrs' 'Value = ['CTR, 'APP]

instance IsExpr 'Example where
  type Hole' 'Example = 'Just Unit
  type Var'  'Example = 'Just Value
  type Ctrs' 'Example = ['CTR, 'APP, 'LAM]

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
  Ctr  :: HasCtr  l   => Ctr -> Expr' r l
  Var  :: HasVar  l v => v -> Expr' r l
  App  :: HasApp  l   => Rec r l -> Rec r l -> Expr' r l
  Lam  :: HasLam  l v => v -> Rec r l -> Expr' r l
  Let  :: HasLet  l v => v -> Rec r l -> Rec r l -> Expr' r l
  Elim :: HasElim l   => [(Ctr, Rec r l)] -> Expr' r l
  Fix  :: HasFix  l   => Expr' r l
  Prj  :: HasPrj  l   => Ctr -> Int -> Expr' r l

data Func' a = Fixed | Base a
  deriving (Eq, Ord, Show, Read)

type Func = Func' Kind.Type

type family Rec (f :: Func) (l :: Level) where
  Rec 'Fixed l = Expr l
  Rec ('Base c) _ = c

type Expr = Expr' 'Fixed
type Base a = Expr' ('Base a)

type Type    = Expr 'Type
type Term h  = Expr ('Term h)
type Value   = Expr 'Value
type Example = Expr 'Example

-- | Indetermine expressions are either a (pattern matching) lambda followed by
-- a term, or a hole.
type Indet = Base (Term Hole) 'Ind

-- | Results are determinate expressions whose holes are indeterminate
-- expressions capturing the local scope.
type Result = Expr 'Det

type Scope = Map Var Result

-- Instances {{{

deriving instance (Consts Eq l, Eq (Rec r l)) => Eq (Expr' r l)
deriving instance (Consts Eq l, Consts Ord l, Ord (Rec r l)) => Ord (Expr' r l)
deriving instance (Consts Show l, Show (Rec r l)) => Show (Expr' r l)

instance
  ( May NFData (Hole' l)
  , May NFData (Var'  l)
  , NFData (Rec r l)
  ) => NFData (Expr' r l) where
  rnf = \case
    Hole h -> rnf h
    Ctr c -> rnf c
    Var v -> rnf v
    App f x -> rnf f `seq` rnf x
    Lam a x -> rnf a `seq` rnf x
    Let a x y -> rnf a `seq` rnf x `seq` rnf y
    Elim xs -> rnf xs
    Fix -> ()
    Prj c n -> rnf c `seq` rnf n

-- }}}

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

-- }}}

-- Smart constructors {{{

pattern Arr :: HasCtr l => HasArr l => Expr l -> Expr l -> Expr l
pattern Arr t u = App (App (Ctr "->") t) u

pattern Case :: () => (HasApp l, HasElim l) =>
  Expr l -> [(Ctr, Expr l)] -> Expr l
pattern Case x xs = App (Elim xs) x

arrs :: (Foldable f, HasArr l) => f (Expr l) -> Expr l
arrs = foldr1 Arr

unArrs :: HasCtr l => Expr l -> NonEmpty (Expr l)
unArrs = \case
  Arr t u -> t `NonEmpty.cons` unArrs u
  t -> pure t

{-# COMPLETE Arrs #-}
pattern Arrs :: HasCtr l => Expr l -> [Expr l] -> Expr l
pattern Arrs a bs <- (unArrs -> (a :| bs))

{-# COMPLETE Args #-}
pattern Args :: HasCtr l => [Expr l] -> Expr l -> Expr l
pattern Args as b <- (unsnoc . unArrs -> (as, b))

lets :: (Foldable f, HasLet l v) => f (v, Expr l) -> Expr l -> Expr l
lets as x = foldr (uncurry Let) x as

unLets :: HasLet l v => Expr l -> ([(v, Expr l)], Expr l)
unLets = \case
  Let a x y -> first ((a, x):) $ unLets y
  t -> pure t

{-# COMPLETE Lets #-}
pattern Lets :: HasLet l v => [(v, Expr l)] -> Expr l -> Expr l
pattern Lets as x <- (unLets -> (as, x))
  where Lets as x = lets as x

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

nat :: (HasCtr l, HasApp l) => Int -> Expr l
nat 0 = Ctr "Zero"
nat n = App (Ctr "Succ") (nat $ n - 1)

unNat :: HasCtr l => Expr l -> Maybe Int
unNat = \case
  Ctr "Zero" -> Just 0
  App (Ctr "Succ") n -> (1+) <$> unNat n
  _ -> Nothing

pattern Nat :: HasCtr l => Int -> Expr l
pattern Nat n <- (unNat -> Just n)

list :: (HasCtr l, HasApp l) => [Expr l] -> Expr l
list = foldr (\x y -> apps (Ctr "Cons") [x, y]) (Ctr "Nil")

unList :: HasCtr l => Expr l -> Maybe [Expr l]
unList = \case
  Ctr "Nil" -> Just []
  Apps (Ctr "Cons") [x, xs] -> (x:) <$> unList xs
  _ -> Nothing

pattern List :: HasCtr l => [Expr l] -> Expr l
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

-- | Fill holes in an expression according to a map of hole fillings.
fill :: (HasHole l h, Ord h) => Map h (Expr l) -> Expr l -> Expr l
fill th = cataExpr \case
  Hole h | Just x <- Map.lookup h th -> x
  e -> fixExpr e

replace :: (Ord v, HasVar l v) => Map v (Expr l) -> Expr l -> Expr l
replace th = cataExpr \case
  Var v | Just x <- Map.lookup v th -> x
  e -> fixExpr e

freeVars :: Term h -> Set Var
freeVars = cataExpr \case
  Lam a x -> Set.delete a x
  Var a -> Set.singleton a
  x -> view rec x

-- | All subexpressions, including the expression itself.
dissect :: Expr l -> [Expr l]
dissect = paraExpr \e -> (e:) . view rec

-- | Check if an expression has no holes.
magic :: Term Void -> Term a
magic = over holes absurd

-- | Check if an expression has no holes.
holeFree :: Term a -> Maybe (Term Void)
holeFree = traverseOf holes $ const Nothing

-- | Extract hole information by mapping to fresh hole variables.
extract :: (Ord h, Count h, MonadState (Fresh h) m) =>
  Term a -> m (Term h, Map h a)
extract x = do
  y <- forOf holes x \h -> (,h) <$> fresh
  return
    ( over holes fst y
    , Map.fromList $ toListOf holes y
    )

-- | Generate the eta-expansion of a type.
eta :: MonadState (Fresh Var) m => Goal -> m (Term Goal)
eta (Goal ctx (Args ts u)) = do
  xs <- for ts \t -> (,t) <$> fresh
  return . lams (fst <$> xs) . Hole . flip Goal u $
    ctx <> Map.fromList (second Mono <$> xs)

-- | Eta expand an expression.
expand :: MonadState (Fresh Var) m => Term Goal -> m (Term Goal)
expand x = forOf holes' x eta

-- | The number of applications in a type.
typeSize :: Type -> Int
typeSize = cataExpr \case
  App f a -> 1 + f + a
  _ -> 0

upcast :: Value -> Result
upcast = cataExpr \case
  Ctr c -> Ctr c
  App f x -> App f x

downcast :: Result -> Maybe Value
downcast = cataExprM \case
  Ctr c -> return $ Ctr c
  App f x -> return $ App f x
  _ -> Nothing

consistent :: Value -> Example -> Bool
consistent = curry \case
  (_, Hole _) -> True
  (Ctr c, Ctr d) -> c == d
  (App f x, App g y) -> consistent f g && consistent x y
  _ -> False

arity :: Poly -> Int
arity (Poly _ (Args as _)) = length as

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

refresh :: MonadState (Fresh Free) m => Poly -> m Poly
refresh (Poly as t) = do
  th <- for as \a -> (a,) <$> fresh
  let u = subst (Var <$> Map.fromList th) t
  return $ Poly (snd <$> th) u

-- | Instantiate all quantified variables of a polytype with fresh variables.
instantiateFresh :: MonadState (Fresh Free) m => Poly -> m Type
instantiateFresh p = do
  Poly _ t <- refresh p
  return t

-- }}}

-- Examples {{{

{-# COMPLETE Top, App, Ctr, Lam #-}
pattern Top :: Example
pattern Top = Hole (Unit ())

-- Merged examples {{{

data Ex
  = ExFun (Map Value Ex)
  | ExCtr Ctr [Ex]
  | ExTop
  deriving stock (Eq, Ord, Show)
  deriving stock Generic

instance NFData Ex

instance PartialSemigroup Ex where
  ExTop <?> ex = Just ex
  ex <?> ExTop = Just ex
  ExFun fs <?> ExFun gs = ExFun <$> fs <?> gs
  ExCtr c xs <?> ExCtr d ys | c == d = ExCtr c <$> partialZip xs ys
  _ <?> _ = Nothing

instance PartialMonoid Ex where
  pempty = ExTop

toEx :: Example -> Ex
toEx = \case
  Top -> ExTop
  Apps (Ctr c) xs -> ExCtr c (toEx <$> xs)
  Lam v x -> ExFun (Map.singleton v $ toEx x)
  _ -> error "Incorrect example"

fromExamples :: [Example] -> Maybe Ex
fromExamples = pfoldMap' toEx

fromEx :: Ex -> [Example]
fromEx = \case
  ExTop -> [Top]
  ExCtr c xs -> apps (Ctr c) <$> for xs fromEx
  ExFun fs -> Map.assocs fs >>= \(v, x) -> Lam v <$> fromEx x

-- }}}

-- }}}

-- TODO: maybe move these definitions somewhere else

newtype Unit = Unit ()
  deriving newtype (Eq, Ord, Show, Read, Semigroup, Monoid, NFData)

data Goal = Goal
  { _goalCtx  :: Map Var Poly
  , _goalType :: Type
  } deriving (Eq, Ord, Show)

makeLenses ''Goal

instance Subst Goal where
  subst th = over goalType (subst th) . over goalCtx (subst th <$>)

data Env = Env
  { _envScope :: Scope
  , _envFunctions :: Map Var Poly
  , _envDatatypes :: Map Ctr ([Free], [(Ctr, [Type])])
  , _envConstructors :: Map Ctr Poly
  , _envForbidden :: [Term Unit]
  } deriving (Eq, Ord)

makeLenses 'Env

instance Semigroup Env where
  Env a c e g i <> Env b d f h j =
    Env (a <> b) (c <> d) (e <> f) (g <> h) (i <> j)

instance Monoid Env where
  mempty = Env mempty mempty mempty mempty mempty

ctrArity :: Env -> Ctr -> Int
ctrArity en c = case Map.lookup c (view envConstructors en) of
  Nothing -> error $ "Unknown constructor " <> show c
  Just d -> arity d

-- Pretty printing {{{

instance Pretty Unit where
  pretty _ = space

instance Pretty Poly where
  pretty = \case
    Poly [] t -> pretty t
    Poly xs t -> "forall" <+> sep (pretty <$> xs) <> dot <+> pretty t

instance Pretty Goal where
  pretty (Goal ts t)
    | null ts = pretty t
    | otherwise = pretty ts <+> "|-" <+> pretty t

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

sTerm :: (HasLam l v, Pretty v) => Sugar l ann
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

sArr :: HasCtr l => Sugar l ann
sArr p i = \case
  Arr t u -> Just . prettyParen (i > 1) $ sep [p 2 t, "->", p 1 u]
  _ -> Nothing

sLit :: HasCtr l => Sugar l ann
sLit p _ = \case
  Nat n -> Just $ pretty n
  List xs -> Just $ P.list (p 0 <$> xs)
  _ -> Nothing

sRes :: HasHole l (Scope, Indet) => Sugar l ann
sRes _ _ = \case
  Hole (m, e)
    | null m -> Just $ pretty e
    | otherwise -> Just $ pretty m <+> "|-" <+> pretty e
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

instance Pretty Ex where
  pretty = pretty . fromEx

instance Pretty Result where
  pretty = withSugar (sLit `orTry` sRes)

instance (Pretty a, Consts Pretty l) => Pretty (Base a l) where
  pretty = pExpr (const pretty) 0

-- }}}
