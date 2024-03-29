module Language.Defs
  ( Signature(..)
  , Binding(..)
  , Datatype(..)
  , Import(..)
  , Pragma(..)
  , Assert(..)
  , Def(..)
  , Defs(..)
  , signatures, bindings, datatypes, imports, pragmas, asserts
  , fromDefs
  , recDefs, relevant
  , evalAssert, unevalAssert
  ) where

import qualified RIO.List as List
import qualified RIO.Map as Map
import qualified RIO.NonEmpty as NonEmpty
import Import
import Language.Syntax
import Language.Live
import Prettyprinter

-- TODO: move this to its own file

data Signature = MkSignature Var Poly
  deriving (Eq, Ord, Show)

data Binding h = MkBinding Var (Term h)
  deriving (Eq, Ord, Show)

data Datatype = MkDatatype Ctr [Free] [(Ctr, [Mono])]
  deriving (Eq, Ord, Show)

data Import = MkImport
  { name :: Text
  , expose :: Maybe [Var]
  } deriving (Eq, Ord, Show)

data Pragma
  = Desc Text
  | Forbid (Term Unit)
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

instance Pretty Import where
  pretty (MkImport name Nothing) = "import" <+> pretty name
  pretty (MkImport name (Just exports)) = "import" <+> pretty name <+>
    parens (mconcat $ List.intersperse ", " (pretty <$> exports))

instance Pretty Datatype where
  pretty (MkDatatype d as cs) =
    "data" <+> sep (pretty d : fmap pretty as) <+>
      ( align . sep . zipWith (<+>) ("=" : List.repeat "|")
      $ cs <&> \(c, xs) -> pretty (Ctr c xs :: Mono)
      )

instance Pretty Signature where
  pretty (MkSignature x t) = pretty x <+> "::" <+> pretty t

instance Pretty h => Pretty (Binding h) where
  pretty (MkBinding x (Lams as e)) =
    sep (pretty x : fmap pretty as) <+> "=" <+> pretty e

instance Pretty Pragma where
  pretty = fancy . \case
    Desc s -> "DESC" <+> pretty (show s)
    Forbid e -> "FORBID" <+> pretty e
    Include xs ->
     let ys = xs <&> \case
           (x, Nothing) -> pretty x
           (x, Just t) -> pretty x <+> "::" <+> pretty t
     in "INCLUDE" <+> fold (NonEmpty.intersperse ", " ys)
    where fancy x = "{-#" <+> x <+> "#-}"

instance Pretty Assert where
  pretty (MkAssert a ex) = "assert" <+> pretty a <+> "<==" <+> pretty ex

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

recBinding :: Binding a -> Binding a
recBinding (MkBinding x e)
  | x `elem` freeVars e = MkBinding x $ App Fix $ Lam x e
  | otherwise = MkBinding x e

recDefs :: Defs a -> Defs a
recDefs (Defs ds) = Defs $ ds <&> \case
  Binding b -> Binding $ recBinding b
  x -> x

relevant :: Term Hole -> [Binding Hole]
relevant (Lets xs _) = [ MkBinding x e | (x, e) <- xs, isNothing (holeFree e) ]

-- TODO: type checking and imports
fromDefs :: Defs Void -> Env
fromDefs defs = forbEnv
  where

    dataEnv, bindEnv, sigEnv, forbEnv :: Env
    dataEnv = foldl' fromData mempty  $ datatypes defs
    bindEnv = foldl' fromBind dataEnv $ bindings defs
    sigEnv  = foldl' fromSigs bindEnv $ signatures defs
    forbEnv = set envForbidden [x | Forbid x <- pragmas defs] sigEnv

    fromData :: Env -> Datatype -> Env
    fromData m (MkDatatype t as cs) = m
      & over envDatatypes (Map.insert t (as, cs))
      & over envConstructors (Map.union cs')
      where
        t' = Ctr t (Var <$> as)
        cs' = Map.fromList cs <&> \ts -> Poly as $ arrs $ ts ++ [t']

    fromBind :: Env -> Binding Void -> Env
    fromBind m (MkBinding x e) =
      let r = runReader (magnify envScope $ eval mempty (magic e)) m
      in m & over envScope (Map.insert x r)

    fromSigs :: Env -> Signature -> Env
    fromSigs m (MkSignature x t) = m & over envFunctions (Map.insert x t)

evalAssert :: MonadReader Scope m => Scope -> Assert -> m (Result, Example)
evalAssert rs (MkAssert e (Lams vs ex)) = do
  v <- eval rs e
  fmap (,ex) . resume mempty $ apps v (upcast <$> vs)

unevalAssert :: Scope -> Assert -> Uneval (Logic Constraints)
unevalAssert m (MkAssert e ex) = do
  r <- liftEval $ eval m e
  uneval r $ toEx ex
