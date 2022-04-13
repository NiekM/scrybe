module Language.Live where

import Import hiding (reverse)
import Language.Syntax
import qualified RIO.Map as Map

live :: Map Var Result -> Term Var Hole -> Maybe Result
live m = let indet x = return . Hole . Annot x $ Scope m in \case
  Var v -> maybe (error $ show v) return $ Map.lookup v m
  App f x -> do
    f' <- live m f
    x' <- live m x
    case f' of
      App Fix (Hole (Annot (Lam g (Lam y e)) (Scope n))) ->
        live (Map.fromList [(g, f'), (y, x')] <> n) e
      Hole (Annot y (Scope n))
        | Lam a z <- y -> live (Map.insert a x' n) z
        | Elim xs <- y
        , Apps (Ctr c) as <- x'
        , Just (Lams bs z) <- lookup c xs
        -> live (Map.fromList (zip bs as) <> m) z
      r -> return $ App r x'
  Ctr c -> return $ Ctr c
  Fix -> return Fix
  Let _a _x _y -> undefined
  -- Indeterminate results
  Hole h  -> indet $ Hole h
  Lam a x -> indet $ Lam a x
  Elim xs -> indet $ Elim xs
