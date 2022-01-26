{-# LANGUAGE RankNTypes #-}
module Lang where

import Import
import GHC.TypeLits
import qualified RIO.Map as Map

-- | Bound type variables are bound locally, i.e. by forall quantifiers
newtype TBound = TBound Int
  deriving stock (Eq, Ord, Read, Show, Data)
  deriving newtype Num
  deriving Pretty via (NumVar "a")

-- | Bound expression variables are bound locally, i.e. by let bindings and
-- lambda abstractions
newtype EBound = EBound Int
  deriving stock (Eq, Ord, Read, Show, Data)
  deriving newtype Num
  deriving Pretty via (NumVar "x")

newtype Hole = Hole Int
  deriving stock (Eq, Ord, Read, Show, Data)
  deriving newtype Num
  deriving Pretty via (NumVar "?")

-- | Free variables are bound globally, i.e. by a type definition, function
-- declaration or imported
newtype Free = Free Text
  deriving stock (Eq, Ord, Read, Show, Data)
  deriving newtype (IsString, Pretty)

-- * Types
data Type
  = TVar (Either TBound Free)
  | TApp Type Type
  deriving stock (Eq, Ord, Read, Show, Data)

pattern TArr :: Type -> Type -> Type
pattern TArr t u = TApp (TApp (TVar (Right (Free "->"))) t) u

-- * Expressions
data Expr
  = ELam EBound Expr
  | EApp Expr Expr
  | EVar (Either EBound Free)
  | EHole Hole
  deriving stock (Eq, Ord, Show, Read, Data)

data Sketch = Sketch
  { expr :: Expr
  , goals :: Map Hole Type
  } deriving stock (Eq, Ord, Read, Show, Data)

-- * Utility functions

-- | Generates all possible ways to apply holes to an expression
expand :: Hole -> Sketch -> Type -> [(Sketch, Type)]
expand n sketch@(Sketch e ts) t = (sketch, t) : case t of
  TArr t1 t2 ->
    expand (1 + n) (Sketch (EApp e (EHole n)) (Map.insert n t1 ts)) t2
  _ -> []


-- * Pretty printing

-- | A helper type for pretty printing variables
newtype NumVar (s :: Symbol) = MkNumVar Int
instance KnownSymbol s => Pretty (NumVar s) where
  pretty var@(MkNumVar n) = fromString (symbolVal var) <> fromString (show n)

prettyParens :: Pretty a => a -> (a -> Bool) -> StyleDoc
prettyParens t p
  | p t = parens (pretty t)
  | otherwise = pretty t

instance Pretty Type where
  pretty = \case
    TArr t u -> sep
      [ prettyParens t \case TArr {} -> True; _ -> False
      , "->"
      , pretty u
      ]
    TVar x -> pretty x
    TApp t u -> sep
      [ pretty t
      , prettyParens u \case TVar {} -> False; _ -> True
      ]

instance Pretty Sketch where
  pretty Sketch { expr, goals } = go expr where
    go = \case
      ELam x e -> "\\" <> pretty x <> "." <+> go e
      EApp f x -> sep
        [ case f of ELam {} -> parens (go f); _ -> go f
        , (case x of EApp {} -> parens; ELam {} -> parens; _ -> id) $ go x
        ]
      EVar x -> pretty x
      EHole i | Just x <- goals Map.!? i ->
        braces $ sep [pretty i, "::", pretty x]
      EHole i -> pretty i

instance Pretty Expr where
  pretty e = pretty (Sketch e mempty)
