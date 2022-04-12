module Language.Live where

import Import hiding (reverse)
import Language.Syntax
import qualified RIO.Map as Map

live :: Map Var Result -> Term Var Hole -> Result
live m = let indet x = Hole (Annot x $ Scope m) in \case
  Var v | Just r <- Map.lookup v m -> r
  App (live m -> f) (live m -> x) -> case f of
    Hole (Annot y (Scope n))
      | Lam a z <- y -> live (Map.insert a x n) z
      | Elim xs <- y
      , Apps (Ctr c) as <- x
      -- TODO: What if the rhs of a branch is not a lambda?
      --       Perhaps that should just never happen, e.g. by eta-expanding
      --       branches during type checking.
      , Just (Lams bs z) <- lookup c xs
      -> live (Map.fromList (zip bs as) <> m) z
    r -> App r x
  Ctr c -> Ctr c
  Var _ -> undefined
  Let _a _x _y -> undefined
  -- Indeterminate results
  Hole h  -> indet $ Hole h
  Lam a x -> indet $ Lam a x
  Elim xs -> indet $ Elim xs
