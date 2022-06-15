module Language.Live where

import Import
import Nondet
import Language.Syntax
import Prettyprinter (list, align, (<+>))
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

-- Examples
type Examples = [Map Hole Ex]

-- TODO: does this example ever make sense?
-- TODO: should we use NonEmpty?
mergeExamples :: Examples -> Examples -> Maybe Examples
mergeExamples as bs = sequence do
  a <- as
  mergeMap mergeEx a <$> bs

-- Deterministic Constraints
type DC_ = Tree (Hole, Ctr) (Map (Hole, Map Var Result) Ex)

data Tree k v
  = Node v
  | Branch (Map k (Tree k v))
  deriving (Eq, Ord, Show)
  deriving (Functor, Foldable, Traversable)

instance (Pretty a, Pretty b) => Pretty (Map a b) where
  pretty m = align . Prettyprinter.list $ Map.assocs m <&> \(k, x) ->
    pretty k <> ":" <+> align (pretty x)

instance (Pretty k, Pretty v) => Pretty (Tree k v) where
  pretty = \case
    Node a -> pretty a
    Branch xs -> pretty xs

instance Pretty Ex where
  pretty = pretty . fromEx

_mergeDC_ :: DC_ -> DC_ -> Maybe DC_
_mergeDC_ (Node x) (Node y) = Node <$> mergeMap mergeEx x y
_mergeDC_ (Branch (null -> True)) y = Just y
_mergeDC_ x (Branch (null -> True)) = Just x
_mergeDC_ (Node x) ys = mapM (mergeMap mergeEx x) ys
_mergeDC_ xs (Node y) = mapM (mergeMap mergeEx y) xs
_mergeDC_ (Branch xs) (Branch ys) = Branch <$> mergeMap _mergeDC_ xs ys

-- -- TODO: first just merge equal scopes
___merge :: [DC_] -> Maybe DC_
___merge = foldl' (\x y -> x >>= _mergeDC_ y) $ Just $ Branch mempty
-- ___merge ds = traceShow (pretty ds) $ foldl' (\x y -> x >>= _mergeDC_ y) (Just $ Branch mempty) ds

___mergeAll :: [Maybe DC_] -> Maybe DC_
___mergeAll = sequence >=> ___merge

___mergeAlt :: [Maybe DC_] -> Maybe DC_
___mergeAlt cs = case catMaybes cs of
  [] -> Nothing
  ds -> ___merge ds

___uneval :: Result -> Ex -> Eval (Maybe DC_)
___uneval = curry \case
  -- Top always succeeds.
  (_, ExTop) -> return . Just $ Branch mempty
  -- Constructors are handled as opaque functions and their unevaluation is
  -- extensional in the sense that only their arguments are compared.
  (Apps (Ctr c) xs, ExCtr d ys) -- TODO: should we include Lams here as well?
    | c == d, length xs == length ys
    -> ___mergeAll <$> zipWithM ___uneval xs ys
  -- Holes are simply added to the environment, with their arguments as inputs.
  -- The arguments should be values in order to obtain correct hole
  -- constraints.
  (Apps (Scoped m (Hole h)) (mapM downcast -> Just vs), ex) -> do
    let ex' = foldr (\v -> ExFun . Map.singleton v) ex vs
    return . Just . Node $ Map.singleton (h, m) ex'
  (App (Prj c n) r, ex) -> do
    cs <- view $ env . constructors -- TODO: something like view constructors
    let ar = arity . fromMaybe (error "Oh oh") $ Map.lookup c cs
    ___uneval r . ExCtr c $ replicate ar ExTop & ix (n - 1) .~ ex
  (App (Scoped m (Elim xs)) r, ex) -> do
    cs <- view $ env . constructors
    let b = fromMaybe undefined $ blocking r
    ___mergeAlt <$> for xs \(c, e) -> do
      let ar = arity . fromMaybe (error "Oh oh") $ Map.lookup c cs
      scrut <- ___uneval r . ExCtr c $ replicate ar ExTop
      let prjs = [App (Prj c n) r | n <- [1..ar]]
      e' <- eval m e
      arm <- resume mempty (apps e' prjs) >>= flip ___uneval ex
      return . fmap (Branch . Map.singleton (b, c)) . ___mergeAll $ [scrut, arm]
  -- Functions should have both input and output, and evaluating their body on
  -- this input should unevaluate onto the output example.
  (Scoped m (Lam a e), ExFun xs) ->
    ___mergeAll <$> for (Map.assocs xs) \(v, ex) -> do
      r <- eval (Map.insert a (upcast v) m) e
      ___uneval r ex
  -- Fixed points additionally add their own definition to the environment.
  (r@(App Fix (Scoped m (Lam f (Indet e)))), ex) ->
    ___uneval (Scoped (Map.insert f r m) e) ex
  _ -> return Nothing

-- TODO: Perhaps throw away normal checkLive in favor of this
___resumeLive :: Map Hole (Term Hole) -> Term Hole -> Constraint -> Eval (Maybe DC_)
___resumeLive hf e cs = ___mergeAll <$> for (Map.assocs cs) \(Scope m, ex) -> do
  r <- eval m e >>= resume hf
  ___uneval r ex

treeAssocs :: Tree k v -> [([k], v)]
treeAssocs = \case
  Node a -> [([], a)]
  Branch xs -> Map.assocs xs >>= \(k, t) -> first (k:) <$> treeAssocs t

treeGrow :: [k] -> Tree k v -> Tree k v
treeGrow ks t = foldr (\k -> Branch . Map.singleton k) t ks

-- TODO: test or prove that this is the correct unevaluation equivalent of
-- resumption, i.e. that resuming and then unevaluation is equivalent to
-- unevaluating and then "un-resuming".
___resumeUneval :: Map Hole (Term Hole) -> DC_ -> Eval (Maybe DC_)
___resumeUneval hf old = do
  foo <- for (treeAssocs old) \(t, xs) -> do
    let t' = filter ((`Map.notMember` hf) . fst) t
    bar <- for (Map.assocs xs) \((h, m), ex) -> do
      m' <- mapM (resume hf) m
      fmap (treeGrow t') <$> case Map.lookup h hf of
        Nothing -> return . Just . Node $ Map.singleton (h, m') ex
        Just e -> do
          r <- eval m' e >>= resume hf
          ___uneval r ex
    return $ ___mergeAll bar
  return . ___mergeAlt $ toList foo

-- -- TODO: test or prove that this is the correct unevaluation equivalent of
-- -- resumption, i.e. that resuming and then unevaluation is equivalent to
-- -- unevaluating and then "un-resuming".
-- ___resumeUneval :: Map Hole (Term Hole) -> DC_ -> Eval (Maybe DC_)
-- ___resumeUneval hf old = do
--   foo <- for old \xs -> do
--     bar <- for (Map.assocs xs) \((h, m), ex) -> do
--       m' <- mapM (resume hf) m
--       case Map.lookup h hf of
--         Nothing -> return . Just . Node $ Map.singleton (h, m') ex
--         Just e -> do
--           r <- eval m' e >>= resume hf
--           ___uneval r ex
--     return $ ___mergeAll bar
--   -- return . ___mergeAlt $ traceShow (pretty foo) $ toList foo
--   return . ___mergeAlt $ toList foo

-- Deterministic Constraints
type DC = Maybe (Map [Ctr] (Map Hole (Map Var Result, Ex)))

-- -- TODO: first just merge equal scopes
__merge :: [DC] -> DC
__merge cs = do
  ds <- sequence cs
  flip mergeMaps ds $ mergeMap \(x, a) (y, b) -> do
    guard (x == y)
    (x,) <$> mergeEx a b

__uneval :: Result -> Ex -> Eval DC
__uneval = curry \case
  -- Top always succeeds.
  (_, ExTop) -> return $ Just mempty
  -- Constructors are handled as opaque functions and their unevaluation is
  -- extensional in the sense that only their arguments are compared.
  (Apps (Ctr c) xs, ExCtr d ys) -- TODO: should we include Lams here as well?
    | c == d, length xs == length ys
    -> __merge <$> zipWithM __uneval xs ys
  -- Holes are simply added to the environment, with their arguments as inputs.
  -- The arguments should be values in order to obtain correct hole
  -- constraints.
  (Apps (Scoped m (Hole h)) (mapM downcast -> Just vs), ex) ->
    return . Just . Map.singleton [] $ Map.singleton h
      (m, foldr (\v -> ExFun . Map.singleton v) ex vs)
  (App (Prj c n) r, ex) -> do
    cs <- view $ env . constructors -- TODO: something like view constructors
    let ar = arity . fromMaybe (error "Oh oh") $ Map.lookup c cs
    __uneval r . ExCtr c $ replicate ar ExTop & ix (n - 1) .~ ex
  (App (Scoped m (Elim xs)) r, ex) -> do
    cs <- view $ env . constructors
    __merge . filter isJust <$> for xs \(c, e) -> do
      let ar = arity . fromMaybe (error "Oh oh") $ Map.lookup c cs
      scrut <- __uneval r . ExCtr c $ replicate ar ExTop
      let prjs = [App (Prj c n) r | n <- [1..ar]]
      e' <- eval m e
      arm <- resume mempty (apps e' prjs) >>= flip __uneval ex
      return . fmap (Map.mapKeys (c:)) $ __merge [scrut, arm]
  -- Functions should have both input and output, and evaluating their body on
  -- this input should unevaluate onto the output example.
  (Scoped m (Lam a e), ExFun xs) ->
    __merge <$> for (Map.assocs xs) \(v, ex) -> do
      r <- eval (Map.insert a (upcast v) m) e
      __uneval r ex
  -- Fixed points additionally add their own definition to the environment.
  (r@(App Fix (Scoped m (Lam f (Indet e)))), ex) ->
    __uneval (Scoped (Map.insert f r m) e) ex
  _ -> return Nothing

-- Constraints
type Cs = Maybe (Map Scope [Map Hole Ex])

-- -- TODO: first just merge equal scopes
_merge :: [Cs] -> Cs
_merge = sequence >=> mergeMaps mergeExamples

_mergeAlt :: [Cs] -> Cs
_mergeAlt cs = case catMaybes cs of
  [] -> Nothing
  xs -> Just $ Map.unionsWith (++) xs

_uneval :: Result -> Ex -> Eval Cs
_uneval = curry \case
  -- Top always succeeds.
  (_, ExTop) -> return $ Just mempty
  -- Constructors are handled as opaque functions and their unevaluation is
  -- extensional in the sense that only their arguments are compared.
  (Apps (Ctr c) xs, ExCtr d ys) -- TODO: should we include Lams here as well?
    | c == d, length xs == length ys
    -> _merge <$> zipWithM _uneval xs ys
  -- Holes are simply added to the environment, with their arguments as inputs.
  -- The arguments should be values in order to obtain correct hole
  -- constraints.
  (Apps (Scoped m (Hole h)) (mapM downcast -> Just vs), ex) ->
    return . Just . Map.singleton (Scope m) . return . Map.singleton h $
      foldr (\v -> ExFun . Map.singleton v) ex vs
  (App (Prj c n) r, ex) -> do
    cs <- view $ env . constructors -- TODO: something like view constructors
    let ar = arity . fromMaybe (error "Oh oh") $ Map.lookup c cs
    _uneval r . ExCtr c $ replicate ar ExTop & ix (n - 1) .~ ex
  (App (Scoped m (Elim xs)) r, ex) -> do
    cs <- view $ env . constructors
    _mergeAlt <$> for xs \(c, e) -> do
      let ar = arity . fromMaybe (error "Oh oh") $ Map.lookup c cs
      scrut <- _uneval r . ExCtr c $ replicate ar ExTop
      let prjs = [App (Prj c n) r | n <- [1..ar]]
      e' <- eval m e
      arm <- resume mempty (apps e' prjs) >>= flip _uneval ex
      return $ _merge [scrut, arm]
  -- Functions should have both input and output, and evaluating their body on
  -- this input should unevaluate onto the output example.
  (Scoped m (Lam a e), ExFun xs) ->
    _merge <$> for (Map.assocs xs) \(v, ex) -> do
      r <- eval (Map.insert a (upcast v) m) e
      _uneval r ex
  -- Fixed points additionally add their own definition to the environment.
  (r@(App Fix (Scoped m (Lam f (Indet e)))), ex) ->
    _uneval (Scoped (Map.insert f r m) e) ex
  _ -> return Nothing

-- TODO: Perhaps throw away normal checkLive in favor of this
_resumeLive :: Map Hole (Term Hole) -> Term Hole -> Constraint -> Eval Cs
_resumeLive hf e cs = _merge <$> for (Map.assocs cs) \(Scope m, ex) -> do
  r <- eval m e >>= resume hf
  _uneval r ex

_removeHoles :: Set Hole -> Cs -> Cs
_removeHoles = fmap . fmap . fmap . flip Map.withoutKeys

-- -- TODO: First fix bug in uneval, then check again if this is correct.
-- _resumeUneval :: Map Hole (Term Hole) -> Cs -> Eval Cs
-- _resumeUneval _ Nothing = return Nothing
-- _resumeUneval hf (Just old) = do
--   updated <- Map.fromListWith mergeExamples <$> for (Map.assocs old)
--     \(Scope m, xs) -> do
--       m' <- for m $ resume hf
--       return (Scope m', xs)
--   new <- for (Map.assocs updated) \(Scope m, xs) -> do
--     _mergeAlt <$> for xs \exs -> do
--       bar <- toList <$> for (Map.intersectionWith (,) hf exs) \(e, ex) -> do
--         r <- eval m e >>= resume hf
--         _uneval r ex
--       return $ _merge bar
--   let combined = _mergeAlt (Just updated : new)
--   return . _removeHoles (Map.keysSet hf) $ combined

_unevalAssert :: Map Var Result -> Assert -> Eval Cs
_unevalAssert m (MkAssert e ex) = do
  r <- eval m e
  _uneval r $ toEx ex
