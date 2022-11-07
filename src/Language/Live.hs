module Language.Live
  ( pattern Scoped
  -- Eval
  , Eval, runEval, liftEval
  , eval, resume
  -- Uneval
  , Uneval, runUneval
  , uneval, resumeUneval
  -- Constraints
  , Constraint, Constraints, mergeConstraints -- TODO: somewhere else?
  , UnevalConstraint(..)
  -- Utils
  , blocking, scrutinizedHole -- TODO: somewhere else
  , recVar, normalizeFilling -- TODO: somewhere else?
  )
  where

import Import
import Language.Syntax
import qualified RIO.Map as Map

{-# COMPLETE Scoped, App, Ctr, Fix, Prj #-}
pattern Scoped :: Scope -> Indet -> Expr' r 'Det
pattern Scoped m e = Hole (m, e)

indet :: Term Hole -> Maybe Indet
indet = \case
  Hole h  -> Just $ Hole h
  Lam a x -> Just $ Lam a x
  Elim xs -> Just $ Elim xs
  _ -> Nothing

{-# COMPLETE Indet, Var, App, Ctr, Let, Fix #-}
pattern Indet :: Indet -> Term Hole
pattern Indet i <- (indet -> Just i) where
  Indet = \case
    Hole h -> Hole h
    Lam a x -> Lam a x
    Elim xs -> Elim xs

-- Live evaluation {{{

type Eval = Reader Scope

runEval :: Env -> Eval a -> a
runEval = flip runReader . view envScope

liftEval :: MonadReader Env m => Eval a -> m a
liftEval x = runReader x <$> view envScope

eval :: MonadReader Scope m => Scope -> Term Hole -> m Result
eval loc = \case
  Var v -> do
    rs <- ask
    maybe (error $ show v) return $ Map.lookup v (loc <> rs)
  App f x -> do
    f' <- eval loc f
    x' <- eval loc x
    evalApp f' x'
  Let a x y -> do
    x' <- eval loc x
    eval (Map.insert a x' loc) y
  Ctr c -> return $ Ctr c
  Fix -> return Fix
  -- Indeterminate results
  Indet i -> return $ Scoped loc i

evalApp :: MonadReader Scope m => Result -> Result -> m Result
evalApp f x = case f of
  App Fix (Scoped m (Lam g (Indet e))) ->
    evalApp (Scoped (Map.insert g f m) e) x
  Scoped m (Lam a y) -> eval (Map.insert a x m) y
  Scoped m (Elim xs)
    | Apps (Ctr c) as <- x
    , Just (Lams bs y) <- lookup c xs
    -> eval (Map.fromList (zip bs as) <> m) y
  -- NOTE: not sure if this is necessary, but simplifying away projections when
  -- possible leads to more readible unevaluation results.
  Prj c n -> resume mempty x >>= \case
    Apps (Ctr d) as | c == d, Just y <- preview (ix (n - 1)) as -> return y
    _ -> return $ App (Prj c n) x
  r -> return $ App r x

normalizeFilling :: Map Hole (Term Hole) -> Map Hole (Term Hole)
normalizeFilling hf
  | hf' == hf = hf'
  | otherwise = normalizeFilling hf'
  where hf' = fill hf <$> hf

-- TODO: Note that resumption loops forever if the hole fillings are (mutually)
-- recursive. An alternative would be to only allow resumption of one hole at a
-- time.
resume :: MonadReader Scope m => Map Hole (Term Hole) -> Result -> m Result
resume hf = cataExprM \case
  App f x -> evalApp f x
  Ctr c   -> return $ Ctr c
  Fix     -> return Fix
  Prj c n -> return $ Prj c n
  -- Indeterminate results
  Scoped m (Hole h)
    | Just x <- Map.lookup h hf
    , x /= Hole h -> resume hf =<< eval m x
  Scoped m e -> do
    m' <- mapM (resume hf) m
    return . Scoped m' $ over rec (fill $ normalizeFilling hf) e

blocking :: Result -> Maybe (Scope, Hole)
blocking = cataExpr \case
  Scoped m (Hole h) -> Just (m, h)
  App f x -> f <|> x
  _ -> Nothing

scrutinizedHole :: Result -> Maybe Hole
scrutinizedHole = \case
  App (Scoped _ (Elim _)) r
    | Just (_, h) <- blocking r
    -> Just h
  App f x -> scrutinizedHole f <|> scrutinizedHole x
  _ -> Nothing

recVar :: Var -> Scope -> Bool
recVar x m = case Map.lookup x m of
  Just (Scoped n _) -> Map.member x n
  _ -> False

-- }}}

-- Live unevaluation {{{

type Uneval = RWST Env () Int Maybe

runUneval :: Env -> Int -> Uneval a -> Maybe a
runUneval x i un = view _1 <$> runRWST un x i

burnFuel :: Uneval ()
burnFuel = get >>= \case
  n | n <= 0    -> fail "Out of fuel"
    | otherwise -> put (n - 1)

-- TODO: find some better names?
type Constraint = Map Scope Ex
type Constraints = Map Hole Constraint

class UnevalConstraint a where
  constr :: Scope -> Hole -> Ex -> a

instance Applicative f => UnevalConstraint (f Constraints) where
  constr m h ex = pure $ Map.singleton h $ Map.singleton m ex

type UnCstr a = (BoundedLattice a, UnevalConstraint a)

-- Non-normalizing variant of uneval
uneval :: UnCstr a => Result -> Ex -> Uneval a
uneval = curry \case
  -- Top always succeeds.
  (_, ExTop) -> return top
  -- Constructors are handled as opaque functions and their unevaluation is
  -- extensional in the sense that only their arguments are compared.
  (r@(Apps (Ctr _) _), ExFun ys)
    -> conj <$> for (Map.assocs ys) \(v, ex) ->
      uneval (App r (upcast v)) ex
  (Apps (Ctr c) xs, ExCtr d ys)
    | c == d, length xs == length ys
    -> conj <$> zipWithM uneval xs ys
  -- Holes are simply added to the environment, with their arguments as inputs.
  -- The arguments should be values in order to obtain correct hole
  -- constraints.
  (Apps (Scoped m (Hole h)) (mapM downcast -> Just vs), ex) -> do
    let ex' = foldr (\v -> ExFun . Map.singleton v) ex vs
    return $ constr m h ex'
  (App (Prj c n) r, ex) -> do
    burnFuel
    ar <- ask <&> flip ctrArity c
    uneval r . ExCtr c $ replicate ar ExTop & ix (n - 1) .~ ex
  (Apps (Scoped m (Elim xs)) (r:rs), ex) -> burnFuel >>
    disj <$> for xs \(c, e) -> do
      ar <- ask <&> flip ctrArity c
      scrut <- uneval r . ExCtr c $ replicate ar ExTop
      let prjs = [App (Prj c n) r | n <- [1..ar]]
      e' <- liftEval (eval m e)
      arm <- liftEval (resume mempty (apps e' (prjs ++ rs))) >>= flip uneval ex
      return $ scrut `and` arm
  (Scoped m (Lam a e), ExFun xs) ->
    conj <$> for (Map.assocs xs) \(v, ex) -> do
      r <- liftEval $ eval (Map.insert a (upcast v) m) e
      uneval r ex
  -- Fixed points additionally add their own definition to the environment.
  (r@(App Fix (Scoped m (Lam f (Indet e)))), ex) ->
    uneval (Scoped (Map.insert f r m) e) ex
  _ -> return bot

resumeLive :: Map Hole (Term Hole) -> Term Hole -> Constraint ->
  Uneval (Logic Constraints)
resumeLive hf e cs = Conjunction <$> for (Map.assocs cs) \(m, ex) -> do
  r <- liftEval $ eval m e >>= resume hf
  uneval r ex

resumeUneval :: Map Hole (Term Hole) -> Logic Constraints ->
  Uneval (Logic Constraints)
resumeUneval hf = fmap join . traverse \old -> do
  -- Update the old constraints by resuming their local scopes and then
  -- merging them.
  foo <- mapM (fromListM (<?>)) <$> for old \c ->
    forOf (each . _1) (Map.assocs c) $ traverseOf each (liftEval . resume hf)
  case foo of
    Nothing -> return $ Disjunction []
    Just upd -> do
      -- Compute the new constraints that arise from filling each hole.
      new <- sequence . toList $ Map.intersectionWith (resumeLive hf) hf upd
      -- Combine the new constraints with the updated constraints, minus the
      -- filled holes.
      return . fmap (Map.\\ hf) . Conjunction $ Pure upd : new

mergeConstraints :: [Constraints] -> Maybe Constraints
mergeConstraints = unionsWithM (<?>)

-- }}}
