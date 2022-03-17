{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Eval where

import Import
import Language.Syntax
import qualified RIO.Map as Map
import Control.Monad.State

class HasEnv a where
  env :: Lens' a (Map Var (Term Void))

instance HasEnv (Map Var (Term Void)) where
  env = id

eval' :: Map Var (Term Void) -> Term Void -> Maybe (Term Void)
eval' m e = evalStateT (eval e) m

-- | A simple evaluator/normalizer for expressions that leaves subexpressions
-- as if when they cannot be evaluated further.
-- TODO: add alpha renaming
eval :: (MonadFail m, MonadState s m, HasEnv s) => Term Void -> m (Term Void)
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
    let ys = xs <&> \(Branch p a) -> (,a) <$> match p y
    case msum ys of
      Nothing -> return $ Case y xs
      Just (m, e) -> do
        modifying env (m <>)
        eval e
  Let a x e -> do
    y <- eval x
    modifying env $ Map.insert a y
    eval e

match :: Pattern Void -> Term a -> Maybe (Map Var (Term a))
match p e = case p of
  Hole h -> absurd h
  Var a -> return $ Map.singleton a e
  Ctr c | Ctr d <- e, c == d -> return Map.empty
  -- Note: pattern matching does not check for duplicate variables in a pattern
  App f x | App g y <- e -> liftM2 (<>) (match f g) (match x y)
  _ -> fail "Pattern match failed"
