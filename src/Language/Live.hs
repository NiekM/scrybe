module Language.Live where

import Import hiding (reverse)
import Language.Syntax
import qualified RIO.Map as Map

live :: Int -> Map Var Result -> Term Var Hole -> Maybe Result
live 0 _ = const Nothing
live i m = let indet x = return . Hole . Annot x $ Scope m in \case
  Var v -> maybe (error $ show v) return $ Map.lookup v m
  App f x -> do
    f' <- live i m f
    x' <- live i m x
    case f' of
      App Fix (Hole (Annot (Lam g (Lam y e)) (Scope n))) ->
        live (i - 1) (Map.fromList [(g, f'), (y, x')] <> n) e
      Hole (Annot y (Scope n))
        | Lam a z <- y -> live (i - 1) (Map.insert a x' n) z
        | Elim xs <- y
        , Apps (Ctr c) as <- x'
        , Just (Lams bs z) <- lookup c xs
        -> live (i - 1) (Map.fromList (zip bs as) <> m) z
      r -> return $ App r x'
  Ctr c -> return $ Ctr c
  Fix -> return Fix
  Let _a _x _y -> undefined
  -- Indeterminate results
  Hole h  -> indet $ Hole h
  Lam a x -> indet $ Lam a x
  Elim xs -> indet $ Elim xs
