{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Eval where

import Import
import Language.Syntax
import Language.Type
import qualified RIO.Map as Map
import Control.Monad.State

class HasEnv a b where
  env :: Lens' a (Map Var (Term b))

-- TODO: unification should be with variables rather than holes

instance HasEnv (Map Var (Term a)) a where
  env = id

eval' :: Map Var (Term Var) -> Term Var -> Maybe (Term Var)
eval' m e = evalStateT (eval e) m

-- | A simple evaluator/normalizer for expressions that leaves subexpressions
-- as if when they cannot be evaluated further.
eval :: (MonadFail m, MonadState s m, HasEnv s Var) =>
  Term Var -> m (Term Var)
eval = \case
  Hole h -> return $ Hole h
  Var x -> do
    m <- use env
    maybe (return $ Var x) eval $ Map.lookup x m
  Ctr c -> return $ Ctr c
  App f x -> do
    g <- eval f
    y <- eval x
    case g of
      Lam a z -> do
        modifying env $ Map.insert a y
        eval z
      _ -> return $ App g y
  Lam a x -> Lam a <$> eval x
  Case x xs -> do
    y <- eval x
    let ys = xs <&> \Branch { pat, arm } -> (,arm) <$> match pat y
    case msum ys of
      Nothing -> return $ Case y xs
      Just (m, e) -> do
        modifying env (m <>)
        eval e

fromPattern :: Pattern Var -> Term Var
fromPattern = \case
  Hole x -> Hole x
  Ctr c -> Ctr c
  App f x -> App (fromPattern f) (fromPattern x)

match :: Pattern Var -> Term Var -> Maybe (Map Var (Term Var))
match = unify . fromPattern
