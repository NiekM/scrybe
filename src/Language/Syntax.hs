{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.Syntax
  ( module Language.Syntax
  , module Language.Syntax.Expr
  , module Language.Syntax.Ident
  , module Language.Syntax.Type
  )
  where

import Import
import Language.Syntax.Expr
import Language.Syntax.Ident
import Language.Syntax.Type
import qualified RIO.Map as Map

-- | Generate the eta-expansion of a type.
eta :: MonadState (Fresh Var) m => Goal -> m (Term Goal)
eta (Goal ctx (Args ts u)) = do
  xs <- for ts \t -> (,t) <$> fresh
  return . lams (fst <$> xs) . Hole . flip Goal u $
    ctx <> Map.fromList (second Mono <$> xs)

-- | Eta expand an expression.
expand :: MonadState (Fresh Var) m => Term Goal -> m (Term Goal)
expand x = forOf holes' x eta

upcast :: Value -> Result
upcast = cataExpr \case
  Ctr c -> Ctr c
  App f x -> App f x

downcast :: Result -> Maybe Value
downcast = cataExpr \case
  Ctr c -> return $ Ctr c
  App f x -> liftM2 App f x
  _ -> Nothing

match :: Term Hole -> Term Unit -> Maybe (Map Hole (Term Unit))
match = curry \case
  (_, Hole _) -> Just mempty
  (Hole h, e) -> Just $ Map.singleton h e
  (Var x, Var y) | x == y -> Just mempty
  (Ctr c, Ctr d) | c == d -> Just mempty
  (App f x, App g y) -> liftM2 (<>) (match f g) (match x y)
  (Lam a x, Lam b y) -> match x (replace (Map.singleton b $ Var a) y)
  _ -> Nothing

consistent :: Value -> Example -> Bool
consistent = curry \case
  (_, Hole _) -> True
  (Ctr c, Ctr d) -> c == d
  (App f x, App g y) -> consistent f g && consistent x y
  _ -> False

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

instance Pretty Ex where
  pretty = pretty . fromEx

-- }}}

data Env = Env
  { _envScope :: Scope
  , _envFunctions :: Map Var Poly
  , _envDatatypes :: Map Ctr ([Free], [(Ctr, [Mono])])
  , _envConstructors :: Map Ctr Poly
  , _envForbidden :: [Term Unit]
  } deriving (Eq, Ord)

makeLenses 'Env

instance Semigroup Env where
  Env a c e g i <> Env b d f h j =
    Env (a <> b) (c <> d) (e <> f) (g <> h) (i <> j)

instance Monoid Env where
  mempty = Env mempty mempty mempty mempty mempty
