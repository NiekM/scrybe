{-# LANGUAGE FlexibleContexts #-}
module Language.Utils where

import Import hiding (reverse)
import Language.Syntax
import qualified RIO.Map as Map
import qualified RIO.Set as Set

splitArgs :: Expr l a -> ([Expr l a], Expr l a)
splitArgs = unsnoc . unArrs

-- | Return all holes in an expression.
holes :: Expr l a -> [a]
holes = toList

free :: Ord a => Type a -> Set a
free = Set.fromList . holes

-- | Uniquely number all holes in an expression.
number :: (Traversable t, MonadFresh n m) => t a -> m (t (n, a))
number = traverse \x -> (,x) <$> fresh

-- | Renumber all holes in an expression.
-- TODO: Rewrite for polytypes and use the quantified type variables to do more
-- efficient renumbering
renumber :: MonadFresh Free m => Expr l Var -> m (Expr l Var)
renumber t = do
  xs <- traverse (\x -> (x,) . return . freeId <$> fresh) (nubOrd $ toList t)
  return $ subst (Map.fromList xs) t

-- | Extract extra information in an expression into a map.
extract :: Ord a => Expr l (a, b) -> (Expr l a, Map a b)
extract = fmap fst &&& Map.fromList . holes

nVar :: Int -> Var
nVar = MkVar . ("a" <>) . fromString . show

instantiate :: Map Var (Type Void) -> Poly -> Poly
instantiate th (Poly fr ty) =
  Poly (filter (`notElem` Map.keys th) fr) (subst th ty)

instantiateFresh :: FreshFree m => Poly -> m (Type Void)
instantiateFresh (Poly xs t) = do
  th <- Map.fromList <$> forM xs \x -> (x,) . Var . freeId <$> fresh
  return $ subst th t

-- | Eta expand all holes in a sketch.
etaExpand :: (FreshVarId m, WithHoleCtxs s m, WithVariables s m) =>
  Term Hole -> m (Term Hole)
etaExpand = fmap join . traverse \i -> do
  ctxs <- use holeCtxs
  case Map.lookup i ctxs of
    Nothing -> return $ Hole i
    Just HoleCtx { goal, local } -> do
      -- Split the type in the arguments and the result type
      let (ts, u) = splitArgs goal
      -- Couple each argument with a fresh name
      xs <- number ts
      let locals' = Map.fromList ((varId &&& id) . fst <$> xs)
      -- Update the hole context
      modifying holeCtxs $ Map.insert i $ HoleCtx u (local <> locals')
      let vars = Map.fromList $ (\(x, t) -> (x, Variable (varId x) t 1 0)) <$> xs
      -- traceShowM vars
      modifying variables (vars <>)
      -- Eta expand the hole
      return $ lams (varId . fst <$> xs) (Hole i)

-- | All subexpressions, including the expression itself.
dissect :: Expr l a -> [Expr l a]
dissect e = e : case e of
  Hole _ -> []
  Var _ -> []
  Ctr _ -> []
  App f x -> dissect f ++ dissect x
  Lam _ x -> dissect x
  Case x xs -> x : concatMap (dissect . arm) xs
  Let _ x y -> dissect x ++ dissect y

-- -- | Normalize a type along with an expression with types in the holes, such
-- -- that the holes are numbered in order of their appearance.
-- normalize :: Type Free -> Term (Type Free) -> (Type Free, Term (Type Free))
-- normalize t e = (subst rename t, subst rename <$> e)
--   where
--     xs = holes t
--     ys = concatMap holes (holes e)
--     -- All holes in order of their appearance, given preference to holes in t
--     zs = nubOrd $ xs ++ ys
--     rename = Map.fromList $ zip zs (Hole . MkFree <$> [0..])
