module Language.Eval where

import Import
import Language.Syntax
import Language.Utils
import RIO.List
import qualified RIO.Map as Map

-- | A simple evaluator/normalizer for expressions that leaves subexpressions
-- as if when they cannot be evaluated further.
eval :: Map Var (Term a) -> Term a -> Term a
eval env = \case
  Hole  h -> Hole h
  Var   x -> fromMaybe (Var x) $ Map.lookup x env
  App f x -> let y = eval env x in case eval env f of
    Lam (Bind b _) z -> eval (Map.insert b y env) z
    Case xs | (Var a :| args) <- unApps y ->
      case find ((a ==) . pat) xs of
        Nothing -> App (Case xs) y
        Just (Branch _ b) -> eval env . apps $ b :| args
    e -> App e y
  Lam b x -> Lam b (eval env x)
  Case xs -> Case (fmap (eval env <$>) xs)
