module Language.Eval where

import Import
import Language.Syntax
import qualified RIO.Map as Map

-- | A simple evaluator/normalizer for expressions that leaves subexpressions
-- as if when they cannot be evaluated further.
eval :: Map Var (Term a) -> Term a -> Term a
eval env = \case
  Hole  h -> Hole h
  Var   x -> fromMaybe (Var x) $ Map.lookup x env
  App f x -> let x' = eval env x in case eval env f of
    Lam (Bind b _) y -> eval (Map.insert b x' env) y
    e -> App e x'
  Lam b x -> Lam b (eval env x)
