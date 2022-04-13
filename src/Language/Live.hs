module Language.Live where

import Import hiding (reverse)
import Language.Syntax
import qualified RIO.Map as Map

evalApp :: Result -> Result -> Result
evalApp f x = case f of
  App Fix (Hole (Annot (Lam g (Lam y e)) (Scope n))) ->
    live (Map.fromList [(g, f), (y, x)] <> n) e
  Hole (Annot y (Scope n))
    | Lam a z <- y -> live (Map.insert a x n) z
    | Elim xs <- y
    , Apps (Ctr c) as <- x
    , Just (Lams bs z) <- lookup c xs
    -> live (Map.fromList (zip bs as) <> n) z
  r -> App r x

live :: Map Var Result -> Term Var Hole -> Result
live m = let indet x = Hole . Annot x $ Scope m in \case
  Var v -> fromMaybe undefined $ Map.lookup v m
  App f x -> evalApp (live m f) (live m x)
  Ctr c -> Ctr c
  Fix -> Fix
  Let _a _x _y -> undefined
  -- Indeterminate results
  Hole h  -> indet $ Hole h
  Lam a x -> indet $ Lam a x
  Elim xs -> indet $ Elim xs

resume :: Map Hole (Term Var Hole) -> Result -> Result
resume hf = \case
  App f x -> evalApp (resume hf f) (resume hf x)
  Ctr c -> Ctr c
  Fix -> Fix
  -- Indeterminate results
  Hole (Annot x (Scope m)) -> case x of
    Hole h -> case Map.lookup h hf of
      Nothing -> withScope (Hole h)
      Just (Hole h') | h == h' -> withScope (Hole h)
      Just f -> resume hf (live m f)
    e -> withScope e
    where withScope e = Hole . Annot e . Scope . fmap (resume hf) $ m
