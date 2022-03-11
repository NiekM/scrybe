{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Eval where

import Import
import Language.Syntax
import Language.Type
import qualified RIO.Map as Map
import Control.Monad.State

class HasEnv a where
  env :: Lens' a (Map Var (Term Var))

-- TODO: unification should be with variables rather than holes

instance HasEnv (Map Var (Term Var)) where
  env = id

eval' :: Map Var (Term Var) -> Term Var -> Maybe (Term Var)
eval' m e = evalStateT (eval e) m

-- | A simple evaluator/normalizer for expressions that leaves subexpressions
-- as if when they cannot be evaluated further.
-- TODO: add alpha renaming
eval :: (MonadFail m, MonadState s m, HasEnv s) => Term Var -> m (Term Var)
eval = \case
  Hole h -> return $ Hole h
  Var x -> do
    m <- use env
    case Map.lookup x m of
      Nothing -> fail $ "Unknown variable " <> show x
      Just e -> eval e
  Ctr c -> return $ Ctr c
  App f x -> do
    g <- eval f
    y <- eval x
    case g of
      Lam a z -> do
        modifying env $ Map.insert a y
        eval z
      _ -> return $ App g y
  Lam a x -> return $ Lam a x
  Case x xs -> do
    y <- eval x
    let ys = xs <&> \Branch { pat, arm } -> (,arm) <$> match pat y
    case msum ys of
      Nothing -> return $ Case y xs
      Just (m, e) -> do
        modifying env (m <>)
        eval e
  Let a x e -> do
    y <- eval x
    modifying env $ Map.insert a y
    eval e

fromPattern :: Pattern Var -> Term Var
fromPattern = \case
  Hole x -> Hole x
  Ctr c -> Ctr c
  App f x -> App (fromPattern f) (fromPattern x)

match :: Pattern Var -> Term Var -> Maybe (Map Var (Term Var))
match = unify . fromPattern
