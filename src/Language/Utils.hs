module Language.Utils where

import Import hiding (reverse)
import Language.Syntax
import Data.Foldable
import qualified RIO.Map as Map
import Control.Monad.State
import RIO.NonEmpty (cons)

-- TODO: replace with more general infix function
arrs :: (HasVar l, HasApp l) => NonEmpty (Expr l a) -> Expr l a
arrs = foldr1 Arr

unArrs :: Expr l a -> NonEmpty (Expr l a)
unArrs = \case
  Arr t u -> t `cons` unArrs u
  t -> pure t

-- | Return all holes in an expression.
holes :: Expr l a -> [a]
holes = toList

-- | Number all holes in an expression.
number :: (Num n, Traversable m) => m a -> State n (m (n, a))
number = traverse \x -> do
  n <- get
  put (n + 1)
  return (n, x)

-- | Replace all holes with numbers and return a mapping from numbers to the
-- initial hole values.
extract :: (Num k, Traversable m, Ord k) => k -> m a -> (m k, Map k a)
extract n t = fmap fst &&& Map.fromList . toList $
  flip evalState n $ number t

nVar :: Int -> Var
nVar = MkVar . ("a" <>) . fromString . show

-- Eta-expand a hole.
eta :: Hole -> Type Free -> State Int (Term Hole, HoleCtx)
eta i ty = do
  let (ts, u) = unsnoc (unArrs ty)
  ys <- fmap (first nVar) <$> number ts
  return (lams (fst <$> ys) (Hole i), (u, Map.fromList ys))

-- Eta-expand all holes in an expression.
etaAll :: Term Hole -> State (Int, Map Hole HoleCtx) (Term Hole)
etaAll = fmap join . traverse \i -> do
  (n, ctxs) <- get
  case Map.lookup i ctxs of
    Nothing -> return $ Hole i
    Just (t, ctx) -> do
      let ((e, (u, ctx')), n') = runState (eta i t) n
      put (n', Map.insert i (u, ctx' <> ctx) ctxs)
      return e

-- | All subexpressions, including the expression itself.
dissect :: Expr l a -> [Expr l a]
dissect e = e : case e of
  Hole _ -> []
  Var _ -> []
  Ctr _ -> []
  App f x -> dissect f ++ dissect x
  Lam _ x -> dissect x
  Case xs -> concatMap (dissect . arm) xs
