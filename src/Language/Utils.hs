module Language.Utils where

import Import hiding (reverse)
import Language.Syntax
import qualified RIO.Map as Map

-- | Split a type into its arguments and the result type.
splitArgs :: expr ~ Expr l a b => expr -> ([expr], expr)
splitArgs = unsnoc . unArrs

-- | Uniquely number all holes in an expression.
number :: (Traversable t, MonadFresh n m) => t a -> m (t (n, a))
number = traverse \x -> (,x) <$> fresh

-- | Extract extra information in an expression into a map.
extract :: Ord b => Expr l a (b, c) -> (Expr l a b, Map b c)
extract = over holes fst &&& Map.fromList . toListOf holes

instantiate :: Map Var (Type Void) -> Poly -> Poly
instantiate th (Poly fr ty) =
  Poly (filter (`notElem` Map.keys th) fr) (subst th ty)

-- | Instantiate all quantified variables of a polytype with fresh variables.
instantiateFresh :: FreshFree m => Poly -> m (Type Void)
instantiateFresh (Poly xs t) = do
  th <- Map.fromList <$> forM xs \x -> (x,) . Var . freeId <$> fresh
  return $ subst th t

-- | Eta expand all holes in a sketch.
etaExpand :: (FreshVarId m, WithHoleCtxs s m, WithVariables s m) =>
  Term Hole -> m (Term Hole)
etaExpand = fmap joinHoles . traverseOf holes \i -> do
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
      let vs = Map.fromList $ xs <&> \(x, t) -> (x, Variable (varId x) t 1 0)
      -- traceShowM vars
      modifying variables (vs <>)
      -- Eta expand the hole
      return $ lams (varId . fst <$> xs) (Hole i)

-- | All subexpressions, including the expression itself.
dissect :: Expr l a b -> [Expr l a b]
dissect e = e : case e of
  Hole _ -> []
  Var _ -> []
  Ctr _ -> []
  App f x -> dissect f ++ dissect x
  Lam _ x -> dissect x
  Let _ x y -> dissect x ++ dissect y
  Case x xs -> x : concatMap (dissect . snd) xs

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
