{-# LANGUAGE UndecidableInstances #-}

module Language.Syntax.Expr
  ( Level'(..)
  , Expr'(.., Arr, Arrs, Args, Case, Lets, Apps, Lams, Top, Scoped)
  , Expr, Mono, Term, Result, Value, Example, Indet
  , Hole', HasHole, HasVar, HasCtr, HasApp
  , apps, lams, arrs
  , rec, fixExpr, cataExpr
  , holes, holes', free, freeVars, extract
  , fill, replace, holeFree, magic
  , Scope
  , nat, list
  ) where

import RIO
import Import
import Language.Syntax.Ident
import Data.Foldable (foldl, foldr1)
import Data.Kind
import qualified RIO.NonEmpty as NonEmpty
import qualified RIO.List as List
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import qualified Prettyprinter as Pretty

-- Levels {{{

data Level' a
  = Type    -- ^ Type level expressions
  | Term a  -- ^ Term level expressions
  | Det     -- ^ Determinate results
  | Ind     -- ^ Indeterminate results
  | Value   -- ^ Concrete values
  | Example -- ^ Input-output examples
  deriving (Eq, Ord, Show, Read)

type Level = Level' Type

data EXPR = CTR | VAR | APP | LAM | LET | ELIM | FIX | PRJ

class IsExpr (l :: Level) where
  type Hole' l :: Maybe Type
  type Hole' l = 'Nothing
  type Var'  l :: Maybe Type
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

-- }}}

-- | The type of expressions, generic in
--
-- r: the type of recursion, used to differentiate between base functors and
-- their fixed point.
--
-- l: the level of the expression, containing both universe levels as well as
-- different compiler stages. This parameter is used to determine which
-- constructors are available, as well as the types of holes and variables in
-- the expression.
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

type Func = Func' Type

type family Rec (f :: Func) (l :: Level) where
  Rec 'Fixed l = Expr l
  Rec ('Base c) _ = c

type Expr = Expr' 'Fixed
type Base a = Expr' ('Base a)

-- | Monotypes are type level expressions without quantifiers.
type Mono = Expr 'Type
instance IsExpr 'Type where
  type Var'  'Type = 'Just Free
  type Ctrs' 'Type = ['VAR, 'CTR, 'APP]

-- | Term level expressions generic in the hole type.
type Term h = Expr ('Term h)
instance IsExpr ('Term h) where
  type Hole' ('Term h) = 'Just h
  type Var'  ('Term h) = 'Just Var
  type Ctrs' ('Term h) = ['VAR, 'CTR, 'APP, 'LAM, 'LET, 'ELIM, 'FIX]

-- | Indetermine expressions are either a (pattern matching) lambda followed by
-- a term, or a hole.
type Indet = Base (Term Hole) 'Ind
instance IsExpr 'Ind where
  type Hole' 'Ind = 'Just Hole
  type Var'  'Ind = 'Just Var
  type Ctrs' 'Ind = ['LAM, 'ELIM]

-- | Results are determinate expressions whose holes are indeterminate
-- expressions capturing the local scope.
type Result = Expr 'Det
instance IsExpr 'Det where
  type Hole' 'Det = 'Just (Scope, Indet)
  type Ctrs' 'Det = ['CTR, 'APP, 'FIX, 'PRJ]

type Scope = Map Var Result

-- | Values are concrete results, consisting of just constructors.
type Value = Expr 'Value
instance IsExpr 'Value where
  type Ctrs' 'Value = ['CTR, 'APP]

-- | Examples represent the expected input-output behaviour of a single
-- execution. Holes represent wildcards (unconstraint output) and variables
-- are used to represent concrete input values.
type Example = Expr 'Example
instance IsExpr 'Example where
  type Hole' 'Example = 'Just Unit
  type Var'  'Example = 'Just Value
  type Ctrs' 'Example = ['CTR, 'APP, 'LAM]

-- Instances {{{

type Consts (c :: Type -> Constraint) (l :: Level) =
  (May c (Hole' l), May c (Var' l))

deriving instance (Consts Eq l, Eq (Rec r l)) => Eq (Expr' r l)
deriving instance (Consts Eq l, Consts Ord l, Ord (Rec r l)) => Ord (Expr' r l)
deriving instance (Consts Show l, Show (Rec r l)) => Show (Expr' r l)

instance (Consts NFData l, NFData (Rec r l)) => NFData (Expr' r l) where
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

-- Helper functions {{{

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

cataExpr :: (Base c l -> c) -> Expr l -> c
cataExpr g = g . over rec (cataExpr g)

fixExpr :: Base (Expr l) l -> Expr l
fixExpr = over rec id

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

free :: Traversal' Mono Free
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

unLets :: HasLet l v => Expr l -> ([(v, Expr l)], Expr l)
unLets = \case
  Let a x y -> first ((a, x):) $ unLets y
  t -> pure t

{-# COMPLETE Lets #-}
pattern Lets :: HasLet l v => [(v, Expr l)] -> Expr l -> Expr l
pattern Lets as x <- (unLets -> (as, x))
  where Lets as x = foldr (uncurry Let) x as

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

{-# COMPLETE Top, App, Ctr, Lam #-}
pattern Top :: Example
pattern Top = Hole Unit

{-# COMPLETE Scoped, App, Ctr, Fix, Prj #-}
pattern Scoped :: Scope -> Indet -> Expr' r 'Det
pattern Scoped m e = Hole (m, e)

-- }}}

-- Pretty printing {{{

prettyParen :: Bool -> Doc ann -> Doc ann
prettyParen b t
  | b = Pretty.parens t
  | otherwise = t

pExpr :: Consts Pretty l =>
  (Int -> Rec r l -> Doc ann) -> Int -> Expr' r l -> Doc ann
pExpr p i = \case
  Hole h -> Pretty.braces $ pretty h
  Var x -> pretty x
  Ctr c -> pretty c
  App f x -> prettyParen (i > 2) $ Pretty.sep [p 2 f, p 3 x]
  Lam a x -> prettyParen (i > 0) $ "\\" <> pretty a <+> "->" <+> p 0 x
  Let a x y -> "let" <+> pretty a <+> "=" <+> p 0 x <+> "in" <+> p 0 y
  Elim xs -> prettyParen (i > 0) $ "\\case" <+> mconcat
    (List.intersperse "; " $ xs <&> \(c, b) -> pretty c <+> "->" <+> p 0 b)
  Fix -> "fix"
  Prj c n -> pretty c <> Pretty.dot <> pretty n

-- Syntax sugar {{{

type Sugar l ann =
  (Int -> Expr l -> Doc ann) -> Int -> Expr l -> Maybe (Doc ann)

orTry :: Sugar l ann -> Sugar l ann -> Sugar l ann
orTry x y p i e = maybe (y p i e) return $ x p i e

sTerm :: (HasLam l v, Pretty v) => Sugar l ann
sTerm p i = \case
  Lam a (Lams as x) -> Just . prettyParen (i > 0) $
    "\\" <> Pretty.sep (pretty <$> a:as) <+> "->" <+> p 0 x
  Let a (Lams as x) e -> Just . prettyParen (i > 0) $
    "let" <+> pretty a <+> Pretty.sep (pretty <$> as)
    <+> "=" <+> p 0 x <+> "in" <+> p 0 e
  Case x xs -> Just . prettyParen (i > 0) $ "case" <+> p 0 x <+> "of" <+>
    mconcat (List.intersperse "; " $ xs <&> \(c, Lams as b) ->
      Pretty.sep (pretty c : fmap pretty as) <+> "->" <+> p 0 b)
  _ -> Nothing

sArr :: HasCtr l => Sugar l ann
sArr p i = \case
  Arr t u -> Just . prettyParen (i > 1) $ Pretty.sep [p 2 t, "->", p 1 u]
  _ -> Nothing

sLit :: HasCtr l => Sugar l ann
sLit p _ = \case
  Nat n -> Just $ pretty n
  List xs -> Just $ Pretty.list (p 0 <$> xs)
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
    "\\" <> Pretty.sep (withSugarPrec sLit 3 <$> v:vs) <+> "->" <+> p 0 x
  _ -> Nothing

withSugarPrec :: Consts Pretty l => Sugar l ann -> Int -> Expr l -> Doc ann
withSugarPrec s n t = fromMaybe (pExpr go n t) (s go n t)
  where go = withSugarPrec s

withSugar :: Consts Pretty l => Sugar l ann -> Expr l -> Doc ann
withSugar s = withSugarPrec s 0

-- }}}

instance Pretty h => Pretty (Term h) where
  pretty = withSugar (sLit `orTry` sTerm)

instance Pretty Mono where
  pretty = withSugar sArr

instance Pretty Value where
  pretty = withSugar sLit

instance Pretty Example where
  pretty = withSugar (sLit `orTry` sExample)

instance Pretty Result where
  pretty = withSugar (sLit `orTry` sRes)

instance (Pretty a, Consts Pretty l) => Pretty (Base a l) where
  pretty = pExpr (const pretty) 0

-- }}}

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
