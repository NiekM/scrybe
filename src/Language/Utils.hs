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

-- | All ways to 'punch' holes into an expression without holes.
punch :: Expr l Void -> [Expr l (Expr l Void)]
punch = punch' . fmap absurd

-- | All ways to 'punch' holes into an expression, flattening any 'recursive'
-- holes.
punch' :: Expr l (Expr l a) -> [Expr l (Expr l a)]
punch' e = Hole (join e) : case e of
  Hole  _ -> []
  Var   x -> [Var x]
  Ctr   c -> [Ctr c]
  App f x -> App <$> punch' f <*> punch' x
  Lam b x -> Lam b <$> punch' x
  -- TODO: this is incorrect ...
  Case xs -> Case . sequenceA <$> (fmap punch' <$> xs)

-- | Replace all holes with numbers and return a mapping from numbers to the
-- initial hole values.
extract :: (Num k, Traversable m, Ord k) => k -> m a -> (m k, Map k a)
extract n t = fmap fst &&& Map.fromList . toList $
  flip evalState n $ number t

-- | Compute all possible ways to replace subexpressions with holes, along with
-- the hole fillings to reverse this.
generalize :: Expr l Void -> Map (Expr l Hole) (Map Hole (Expr l Void))
generalize = Map.fromList . fmap (extract 0) . punch

type HoleCtx = (Type Hole, Map Var (Type Hole))

nVar :: Int -> Var
nVar = MkVar . ("a" <>) . fromString . show

-- Eta-expand a hole.
eta :: Hole -> Type Hole -> State Int (Term Hole, HoleCtx)
eta i ty = do
  let (ts, u) = unsnoc (unArrs ty)
  ys <- fmap (first nVar) <$> number ts
  return (lams (uncurry Bind <$> ys) (Hole i), (u, Map.fromList ys))

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

-- | Compute the contexts for each hole in a sketch.
holeContexts :: Map Var (Type Hole) -> Term a
  -> Term (a, Map Var (Type Hole))
holeContexts env = \case
  Lam (Bind x t) e -> Lam (Bind x t) $ holeContexts (Map.insert x t env) e
  App x y -> App (holeContexts env x) (holeContexts env y)
  Case xs -> Case $ fmap (holeContexts env) <$> xs
  t -> (,env) <$> t
