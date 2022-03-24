module Language.Utils where

import Import hiding (reverse)
import Language.Syntax
import qualified RIO.Map as Map

-- | Split a type into its arguments and the result type.
splitArgs :: Expr l v h -> ([Expr l v h], Expr l v h)
splitArgs = unsnoc . unArrs

-- | Uniquely number all holes in an expression.
number :: (Traversable t, MonadFresh n m) => t a -> m (t (n, a))
number = traverse \x -> (,x) <$> fresh

-- | Extract extra information in an expression into a map.
extract :: Ord h => Expr l v (h, c) -> (Expr l v h, Map h c)
extract = over holes fst &&& Map.fromList . toListOf holes

-- TODO: instantiation should also be able to introduce new type variables,
-- e.g. by instantiating `id` to `forall a. List a -> List a`
instantiate :: Map Var Type -> Poly -> Poly
instantiate th (Poly fr ty) =
  Poly (filter (`notElem` Map.keys th) fr) (subst th ty)

-- | Instantiate all quantified variables of a polytype with fresh variables.
instantiateFresh :: FreshFree m => Poly -> m Type
instantiateFresh (Poly xs t) = do
  th <- Map.fromList <$> forM xs \x -> (x,) . Var . freeId <$> fresh
  return $ subst th t

-- | Eta expand all holes in a sketch.
etaExpand :: (FreshVarId m, WithHoleCtxs s m, WithVariables s m) =>
  Term Var Hole -> m (Term Var Hole)
etaExpand = fmap (over holes' id) . traverseOf holes \i -> do
  ctxs <- use holeCtxs
  case Map.lookup i ctxs of
    Nothing -> return $ Hole i
    Just ctx -> do
      -- Split the type in the arguments and the result type
      let (ts, u) = splitArgs (view goal ctx)
      -- Couple each argument with a fresh name
      xs <- number ts
      let locals' = Map.fromList ((varId &&& id) . fst <$> xs)
      -- Update the hole context
      modifying holeCtxs $ Map.insert i $ HoleCtx u (view local ctx <> locals')
      let vs = Map.fromList $ xs <&> \(x, t) -> (x, Variable (varId x) t 1 0)
      -- traceShowM vars
      modifying variables (vs <>)
      -- Eta expand the hole
      return $ lams (varId . fst <$> xs) (Hole i)

-- | All subexpressions, including the expression itself.
dissect :: Expr l v h -> [Expr l v h]
dissect = paraExpr \e -> (e:) . \case
  Hole _ -> mempty
  Var _ -> mempty
  Ctr _ -> mempty
  App f x -> f <> x
  Lam _ x -> x
  Let _ x y -> x <> y
  Case x xs -> x <> mconcat (snd <$> xs)

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
