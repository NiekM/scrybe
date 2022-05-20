module Language.Live where

import Import hiding (reverse)
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

{-# COMPLETE Indet, Var, App, Ctr, Fix #-}
pattern Indet :: Indet -> Term Hole
pattern Indet i <- (indet -> Just i) where
  Indet = \case
    Hole h -> Hole h
    Lam a x -> Lam a x
    Elim xs -> Elim xs

-- Live evaluation {{{

eval :: MonadReader Mod m => LiveEnv -> Term Hole -> m Result
eval loc = \case
  Var v -> do
    env <- live_ <$> ask
    maybe undefined return $ Map.lookup v (loc <> env)
  App f x -> do
    f' <- eval loc f
    x' <- eval loc x
    evalApp f' x'
  Ctr c -> return $ Ctr c
  Fix -> return Fix
  -- Indeterminate results
  Indet i -> return $ Scoped loc i

evalApp :: MonadReader Mod m => Result -> Result -> m Result
evalApp f x = case f of
  App Fix (Scoped m (Lam g (Indet e))) ->
    evalApp (Scoped (Map.insert g f m) e) x
  -- NOTE: this performs only a single recursive step, which is not entirely in
  -- line with evaluation semantics but is nice for testing functions that
  -- would loop infinitely. This also allows functions defined as a fixed point
  -- to not use the recursive argument, which is useful for automatic insertion
  -- of fixed points in let desugaring. Eventually this should be replaced by a
  -- lazy form of live evaluation.
  -- Fix | Scoped m (Lam g e) <- x -> eval (Map.insert g (App f x) m) e
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
resume :: MonadReader Mod m => Map Hole (Term Hole) -> Result -> m Result
resume hf = cataExprM \case
  App f x -> evalApp f x
  Ctr c -> return $ Ctr c
  Fix -> return Fix
  Prj c n -> return $ Prj c n
  -- Indeterminate results
  Scoped m (Hole h)
    | Just x <- Map.lookup h hf
    , x /= Hole h -> resume hf =<< eval m x
  Scoped m e -> do
    m' <- mapM (resume hf) m
    return $ Scoped m' e

blocking :: Result -> Maybe Hole
blocking = cataExpr \case
  Scoped _ (Hole h) -> Just h
  App f x -> f <|> x
  _ -> Nothing

-- }}}

-- TODO: type checking and imports
fromDefs :: Defs Void -> Mod
fromDefs defs = foldl' fromSigs bindMod ss
  where
    dataMod :: Mod
    dataMod = foldl' fromData mempty ds

    bindMod :: Mod
    bindMod = foldl' fromBind dataMod bs

    fromData :: Mod -> Datatype -> Mod
    fromData m (MkDatatype t as cs) = m
      { data_ = Map.insert t (as, cs) (data_ m)
      , ctrs_ = Map.union cs' (ctrs_ m)
      } where
        t' = apps (Ctr t) (Var <$> as)
        cs' = Map.fromList cs <&> \ts -> Poly as $ arrs $ ts ++ [t']

    fromBind :: Mod -> Binding Void -> Mod
    fromBind m (MkBinding x e) =
      let live = live_ m
          r = runReader (eval mempty (over holes absurd e)) m
      in m { live_ = Map.insert x r live }

    fromSigs :: Mod -> Signature -> Mod
    fromSigs m (MkSignature x t) = m { funs_ = Map.insert x t (funs_ m) }

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

type Constraint = Map Scope Ex

class HasCstr a where
  constraints :: Lens' a (Map Hole Constraint)

type Uneval = Map Hole Constraint

-- Live unevaluation {{{

type MonadUneval m = (MonadPlus m, MonadReader Mod m)

mergeUneval :: [Uneval] -> Maybe Uneval
mergeUneval = mergeMaps $ mergeMap mergeEx

checkLive :: MonadUneval m => Term Hole -> Constraint -> m Uneval
checkLive e cs = do
  mfold . mergeUneval =<< for (Map.assocs cs) \(Scope m, ex) -> do
    x <- mfold . fromEx $ ex
    eval m e >>= flip uneval x

-- TODO: maybe replace Lam by Fix and have Lam be a pattern synonym that sets
-- the recursive argument to Nothing. This makes many things more messy
-- unfortunately.
-- TODO: add fuel (probably using a FuelMonad or something, so that we can
-- leave it out as well)
uneval :: MonadUneval m => Result -> Example -> m Uneval
uneval = curry \case
  -- Top always succeeds.
  (_, Top) -> return mempty
  -- Constructors are handled as opaque functions and their unevaluation is
  -- extensional in the sense that only their arguments are compared.
  (Apps (Ctr c) xs, Lams ys (Apps (Ctr d) zs))
    | c == d, length xs + length ys == length zs
    -> mfold . mergeUneval =<< zipWithM uneval (xs ++ map upcast ys) zs
  -- Holes are simply added to the environment, with their arguments as inputs.
  -- The arguments should be values in order to obtain correct hole
  -- constraints.
  (Apps (Scoped m (Hole h)) (mapM downcast -> Just vs), ex) ->
    return $ Map.singleton h $ Map.singleton (Scope m) $ toEx $ lams vs ex
  (App (Prj c n) r, ex) -> do
    cs <- ctrs_ <$> ask
    let ar = arity . fromMaybe (error "Oh oh") . Map.lookup c $ cs
    uneval r $ apps (Ctr c) (replicate ar Top & ix (n - 1) .~ ex)
  (App (Scoped m (Elim xs)) r, ex) -> do
    (c, e) <- mfold xs
    cs <- ctrs_ <$> ask
    let ar = arity . fromMaybe (error "Oh oh") $ Map.lookup c cs
    scrut <- uneval r (apps (Ctr c) (replicate ar Top))
    let prjs = [App (Prj c n) r | n <- [1..ar]]
    e' <- eval m e
    arm <- resume mempty (apps e' prjs) >>= flip uneval ex
    mfold $ mergeUneval [scrut, arm]
  -- Functions should have both input and output, and evaluating their body on
  -- this input should unevaluate onto the output example.
  (Scoped m (Lam a x), Lam v y) ->
    checkLive x $ Map.singleton (Scope $ Map.insert a (upcast v) m) (toEx y)
  -- Fixed points additionally add their own definition to the environment.
  (r@(App Fix (Scoped m (Lam f (Indet e)))), ex) ->
    uneval (Scoped (Map.insert f r m) e) ex
  _ -> mzero

-- TODO: simplify these helper functions

unevalEx :: MonadUneval m => Result -> Ex -> m Uneval
unevalEx r ex = mfold . mergeUneval =<< for (fromEx ex) (uneval r)

unevals :: MonadUneval m => Term Hole -> Constraint -> m Uneval
unevals e cs = mfold . mergeUneval =<< for (Map.assocs cs)
  \(Scope s, x) -> eval s e >>= flip unevalEx x

unevalsFill :: MonadUneval m =>
  Map Hole (Term Hole) -> Constraint -> m Constraint
unevalsFill hf cs = do
  bar <- for (Map.assocs cs) \(Scope s, x) -> do
    s' <- mapM (resume hf) s
    return $ Map.singleton (Scope s') x
  mfold $ mergeMaps mergeEx bar

-- TODO: test or prove that this is the correct unevaluation equivalent of
-- resumption, i.e. that resuming and then unevaluation is equivalent to
-- unevaluating and then "un-resuming".
resumeUneval :: MonadUneval m => Hole -> Term Hole -> Uneval -> m Uneval
resumeUneval h e un = do
  c <- mfold $ Map.lookup h un
  -- TODO: should we call unevalsFill on c?
  x <- unevalsFill (Map.singleton h e) c >>= unevals e
  y <- for (Map.delete h un) $ unevalsFill $ Map.singleton h e
  mfold $ mergeUneval [x, y]

-- }}}
