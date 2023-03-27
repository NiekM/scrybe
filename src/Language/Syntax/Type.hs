{-# LANGUAGE TemplateHaskell #-}

module Language.Syntax.Type
  ( Poly(.., Mono), poly
  , Goal(Goal), goalCtx, goalType
  , Subst(..)
  , freeze, freezeAll, freezeUnbound
  , instantiate, refresh, instantiateFresh
  , typeSize
  ) where

import Import
import Language.Syntax.Expr
import Language.Syntax.Ident
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import qualified Prettyprinter as Pretty

data Poly = Poly [Free] Mono
  deriving (Eq, Ord, Show)

pattern Mono :: Mono -> Poly
pattern Mono t = Poly [] t

-- | Turn a monotype into a polytype by quantifying all its free variables.
poly :: Mono -> Poly
poly t = Poly (nubOrd $ toListOf free t) t

instance Pretty Poly where
  pretty = \case
    Poly [] t -> pretty t
    Poly xs t ->
      "forall" <+> Pretty.sep (pretty <$> xs) <> Pretty.dot <+> pretty t

data Goal = Goal
  { _goalCtx  :: Map Var Poly
  , _goalType :: Mono
  } deriving (Eq, Ord, Show)

instance Pretty Goal where
  pretty (Goal ts t)
    | null ts = pretty t
    | otherwise = pretty ts <+> "|-" <+> pretty t

makeLenses ''Goal

class Subst a where
  subst :: Map Free Mono -> a -> a

instance Subst Mono where
  subst th = cataExpr \case
    Var v | Just x <- Map.lookup v th -> x
    e -> fixExpr e

instance Subst Poly where
  subst th (Poly as t) =
    Poly as $ subst (Map.withoutKeys th (Set.fromList as)) t

instance Subst Goal where
  subst th = over goalType (subst th) . over goalCtx (subst th <$>)

-- Helper functions

-- TODO: replace freezing with something more reasonable
-- | Turn a polytype into a monotype by turning all quantified variables in
-- constructors of the same name.
freeze :: Poly -> Mono
freeze (Poly as t) = flip cataExpr t \case
  Var v | v `elem` as, MkFree c <- v -> Ctr (MkCtr c) []
  e -> fixExpr e

freezeUnbound :: Poly -> Poly
freezeUnbound (Poly as t) = Poly as $ flip cataExpr t \case
  Var v | v `notElem` as, MkFree c <- v -> Ctr (MkCtr c) []
  e -> fixExpr e

freezeAll :: Mono -> Mono
freezeAll = cataExpr \case
  Var (MkFree c) -> Ctr (MkCtr c) []
  e -> fixExpr e

-- TODO: instantiation should also be able to introduce new type variables,
-- e.g. by instantiating `id` to `forall a. List a -> List a`. Perhaps we
-- should just substitute the monotype and recompute free quantified variables.
instantiate :: Map Free Mono -> Poly -> Poly
instantiate th (Poly fr ty) =
  Poly (filter (`notElem` Map.keys th) fr) (subst th ty)

refresh :: MonadState (Fresh Free) m => Poly -> m Poly
refresh (Poly as t) = do
  th <- for as \a -> (a,) <$> fresh
  let u = subst (Var <$> Map.fromList th) t
  return $ Poly (snd <$> th) u

-- | Instantiate all quantified variables of a polytype with fresh variables.
instantiateFresh :: MonadState (Fresh Free) m => Poly -> m Mono
instantiateFresh p = do
  Poly _ t <- refresh p
  return t

-- | The number of applications in a type.
typeSize :: Mono -> Int
typeSize = cataExpr \case
  Arr f a -> 1 + f + a
  Ctr _ xs -> 1 + sum xs
  _ -> 0
