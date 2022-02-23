{-# LANGUAGE FlexibleContexts #-}
module Language.Utils where

import Import hiding (reverse)
import Language.Syntax
import Data.Foldable
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import RIO.NonEmpty (cons)

-- TODO: replace with more general infix function
arrs :: (HasVar l, HasApp l) => NonEmpty (Expr l a) -> Expr l a
arrs = foldr1 Arr

unArrs :: Expr l a -> NonEmpty (Expr l a)
unArrs = \case
  Arr t u -> t `cons` unArrs u
  t -> pure t

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
renumber :: (Ord n, Monad t, Traversable t, MonadFresh n m) => t n -> m (t n)
renumber t = do
  xs <- traverse (\x -> (x,) . return <$> fresh) (nubOrd $ toList t)
  return $ subst (Map.fromList xs) t

-- | Extract extra information in an expression into a map.
extract :: Ord a => Expr l (a, b) -> (Expr l a, Map a b)
extract = fmap fst &&& Map.fromList . holes

nVar :: Int -> Var
nVar = MkVar . ("a" <>) . fromString . show

-- | Eta expand all holes in a sketch.
etaExpand :: (MonadFresh Var m, MonadState s m, HasHoleCtxs s) =>
  Term Hole -> m (Term Hole)
etaExpand = fmap join . traverse \i -> do
  ctxs <- use holeCtxs
  case Map.lookup i ctxs of
    Nothing -> return $ Hole i
    Just HoleCtx { goal, local } -> do
      -- Split the type in the arguments and the result type
      let (ts, u) = splitArgs goal
      -- Couple each argument with a fresh name
      ys <- number ts
      -- Update the hole context
      modifying holeCtxs $ Map.insert i (HoleCtx u (local <> Map.fromList ys))
      -- Eta expand the hole
      return $ lams (fst <$> ys) (Hole i)

-- | All subexpressions, including the expression itself.
dissect :: Expr l a -> [Expr l a]
dissect e = e : case e of
  Hole _ -> []
  Var _ -> []
  Ctr _ -> []
  App f x -> dissect f ++ dissect x
  Lam _ x -> dissect x
  Case xs -> concatMap (dissect . arm) xs
