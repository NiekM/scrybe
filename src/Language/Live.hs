module Language.Live where

import Import
import Nondet
import Language.Syntax
import qualified RIO.Map as Map

{-# COMPLETE Scoped, App, Ctr, Fix, Prj #-}
pattern Scoped :: LiveEnv -> Indet -> Expr' r 'Det
pattern Scoped m e = Hole (Annot e (Scope m))

{-# COMPLETE Top, App, Ctr, Lam #-}
pattern Top :: Example
pattern Top = Hole (Unit ())

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

type Eval = Reader Env

runEval :: Env -> Eval a -> a
runEval = flip runReader

class LiftEval m where
  liftEval :: Eval a -> m a

eval :: LiveEnv -> Term Hole -> Eval Result
eval loc = \case
  Var v -> do
    rs <- view liveEnv
    maybe undefined return $ Map.lookup v (loc <> rs)
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

evalApp :: Result -> Result -> Eval Result
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

-- TODO: Note that resumption loops forever if the hole fillings are (mutually)
-- recursive. An alternative would be to only allow resumption of one hole at a
-- time.
resume :: Map Hole (Term Hole) -> Result -> Eval Result
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
    return . Scoped m' $ over rec (fill hf) e

evalAssert :: Map Var Result -> Assert -> Eval (Result, Example)
evalAssert rs (MkAssert e (Lams vs ex)) = do
  v <- eval rs e
  fmap (,ex) . resume mempty $ apps v (upcast <$> vs)

blocking :: Result -> Maybe Hole
blocking = cataExpr \case
  Scoped _ (Hole h) -> Just h
  App f x -> f <|> x
  _ -> Nothing

-- }}}

-- TODO: type checking and imports
fromDefs :: Defs Void -> Env
fromDefs defs = foldl' fromSigs bindEnv ss
  where
    dataEnv :: Env
    dataEnv = foldl' fromData mempty ds

    bindEnv :: Env
    bindEnv = foldl' fromBind dataEnv bs

    fromData :: Env -> Datatype -> Env
    fromData m (MkDatatype t as cs) = m
      & over dataTypes (Map.insert t (as, cs))
      & over constructors (Map.union cs')
      where
        t' = apps (Ctr t) (Var <$> as)
        cs' = Map.fromList cs <&> \ts -> Poly as $ arrs $ ts ++ [t']

    fromBind :: Env -> Binding Void -> Env
    fromBind m (MkBinding x e) =
      let r = runReader (eval mempty (over holes absurd e)) m
      in m & over liveEnv (Map.insert x r)

    fromSigs :: Env -> Signature -> Env
    fromSigs m (MkSignature x t) = m & over functions (Map.insert x t)

    (_, ss, bs, ds, _) = sepDefs defs

upcast :: Value -> Result
upcast = cataExpr \case
  Ctr c -> Ctr c
  App f x -> App f x

downcast :: Result -> Maybe Value
downcast = cataExprM \case
  Ctr c -> return $ Ctr c
  App f x -> return $ App f x
  _ -> Nothing

-- Merged examples {{{

data Ex
  = ExFun (Map Value Ex)
  | ExCtr Ctr [Ex]
  | ExTop
  deriving (Eq, Ord, Show)

mergeEx :: Ex -> Ex -> Maybe Ex
mergeEx = curry \case
  (ExTop, ex) -> Just ex
  (ex, ExTop) -> Just ex
  (ExFun fs, ExFun gs) -> ExFun <$> mergeMap mergeEx fs gs
  (ExCtr c xs, ExCtr d ys) | c == d, length xs == length ys ->
    ExCtr c <$> zipWithM mergeEx xs ys
  _ -> Nothing

toEx :: Example -> Ex
toEx = \case
  Top -> ExTop
  Apps (Ctr c) xs -> ExCtr c (toEx <$> xs)
  Lam v x -> ExFun (Map.singleton v $ toEx x)
  _ -> error "Incorrect example"

fromExamples :: [Example] -> Maybe Ex
fromExamples = Just ExTop & foldl' \r x -> r >>= mergeEx (toEx x)

fromEx :: Ex -> [Example]
fromEx = \case
  ExTop -> [Top]
  ExCtr c xs -> apps (Ctr c) <$> for xs fromEx
  ExFun fs -> Map.assocs fs >>= \(v, x) -> Lam v <$> fromEx x

-- }}}

-- Live unevaluation {{{

type Uneval = RWST Env () Int Nondet

instance LiftEval Uneval where
  liftEval x = ask <&> runReader x

burnFuel :: Uneval ()
burnFuel = get >>= \case
  n | n <= 0    -> fail "Out of fuel"
    | otherwise -> put (n - 1)

-- TODO: find some better names?
type Constraint = Map Scope Ex
type Constraints = Map Hole Constraint

mergeConstraints :: MonadPlus m => [Constraints] -> m Constraints
mergeConstraints = mfold . mergeMaps (mergeMap mergeEx)

live :: Term Hole -> Constraint -> Uneval Constraints
live e cs =
  mergeConstraints =<< for (Map.assocs cs) \(Scope m, ex) -> do
    x <- mfold . fromEx $ ex
    r <- liftEval (eval m e)
    uneval r x

-- TODO: maybe replace Lam by Fix and have Lam be a pattern synonym that sets
-- the recursive argument to Nothing. This makes many things more messy
-- unfortunately.
uneval :: Result -> Example -> Uneval Constraints
uneval = curry \case
  -- Top always succeeds.
  (_, Top) -> return mempty
  -- Constructors are handled as opaque functions and their unevaluation is
  -- extensional in the sense that only their arguments are compared.
  (Apps (Ctr c) xs, Lams ys (Apps (Ctr d) zs))
    | c == d, length xs + length ys == length zs
    -> mergeConstraints =<< zipWithM uneval (xs ++ map upcast ys) zs
  -- Holes are simply added to the environment, with their arguments as inputs.
  -- The arguments should be values in order to obtain correct hole
  -- constraints.
  (Apps (Scoped m (Hole h)) (mapM downcast -> Just vs), ex) ->
    return $ Map.singleton h $ Map.singleton (Scope m) $ toEx $ lams vs ex
  (App (Prj c n) r, ex) -> do
    cs <- view $ env . constructors -- TODO: something like view constructors
    let ar = arity . fromMaybe (error "Oh oh") . Map.lookup c $ cs
    uneval r $ apps (Ctr c) (replicate ar Top & ix (n - 1) .~ ex)
  (App (Scoped m (Elim xs)) r, ex) -> burnFuel >> do
    (c, e) <- mfold xs
    cs <- view $ env . constructors
    let ar = arity . fromMaybe (error "Oh oh") $ Map.lookup c cs
    scrut <- uneval r (apps (Ctr c) (replicate ar Top))
    let prjs = [App (Prj c n) r | n <- [1..ar]]
    e' <- liftEval $ eval m e
    arm <- liftEval (resume mempty $ apps e' prjs) >>= flip uneval ex
    mergeConstraints [scrut, arm]
  -- Functions should have both input and output, and evaluating their body on
  -- this input should unevaluate onto the output example.
  (Scoped m (Lam a x), Lam v y) ->
    live x $ Map.singleton (Scope $ Map.insert a (upcast v) m) (toEx y)
  -- Fixed points additionally add their own definition to the environment.
  (r@(App Fix (Scoped m (Lam f (Indet e)))), ex) ->
    uneval (Scoped (Map.insert f r m) e) ex
  _ -> mzero

-- TODO: Perhaps throw away normal checkLive in favor of this
resumeLive :: Map Hole (Term Hole) -> Term Hole -> Constraint ->
  Uneval Constraints
resumeLive hf e cs =
  mergeConstraints =<< for (Map.assocs cs) \(Scope m, ex) -> do
    x <- mfold . fromEx $ ex
    r <- liftEval (eval m e >>= resume hf)
    uneval r x

-- TODO: test or prove that this is the correct unevaluation equivalent of
-- resumption, i.e. that resuming and then unevaluation is equivalent to
-- unevaluating and then "un-resuming".
resumeUneval :: Map Hole (Term Hole) -> Constraints -> Uneval Constraints
resumeUneval hf old = do
  -- Update the old constraints by resuming their local scopes and then
  -- merging them.
  updated <- mfold . mapM (mergeFromAssocs mergeEx) =<< for old \c ->
    forOf (each . _1 . scope) (Map.assocs c) $ mapM (liftEval . resume hf)
  -- Compute the new constraints that arise from filling each hole.
  -- TODO: check that this line is correct now. Can we optimize it?
  new <- sequence . toList $ Map.intersectionWith (resumeLive hf) hf updated
  -- Combine the new constraints with the updated constraints, minus the filled
  -- holes.
  mergeConstraints . fmap (Map.\\ hf) $ updated : new

unevalAssert :: Map Var Result -> Assert -> Uneval Constraints
unevalAssert m (MkAssert e ex) = do
  r <- liftEval $ eval m e
  uneval r ex

-- }}}
