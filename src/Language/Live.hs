module Language.Live where

import Import hiding (reverse)
import Language.Syntax
import Language.Type
import RIO.List
import qualified RIO.Map as Map

{-# COMPLETE Scoped, App, Ctr, Fix #-}
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
pattern Indet i <- (indet -> Just i)

-- Live evaluation {{{

-- TODO: move env to reader monad
evalApp :: MonadReader Mod m => Result -> Result -> m Result
evalApp f x = case f of
  App Fix (Scoped m (Lam g (Indet e))) ->
    evalApp (Scoped (Map.insert g f m) e) x
  Scoped m (Lam a y) -> eval (Map.insert a x m) y
  Scoped m (Elim xs)
    | Apps (Ctr c) as <- x
    , Just (Lams bs y) <- lookup c xs
    -> eval (Map.fromList (zip bs as) <> m) y
  r -> return $ App r x

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

-- TODO: type checking and imports
fromDfs :: Defs Void -> Mod
fromDfs defs = foldl' fromSigs bindMod ss
  where
    dataMod :: Mod
    dataMod = foldl' fromData mempty ds

    bindMod :: Mod
    bindMod = foldl' fromBind dataMod bs

    fromData :: Mod -> Datatype -> Mod
    fromData m (MkDatatype t as cs) =
      m { data_ = Map.insert t (as, cs) (data_ m) }

    fromBind :: Mod -> Binding Void -> Mod
    fromBind m (MkBinding x e) =
      let live = live_ m
          r = runReader (eval mempty (over holes absurd e)) m
      in m { live_ = Map.insert x r live }

    fromSigs :: Mod -> Signature -> Mod
    fromSigs m (MkSignature x t) =
      m { funs_ = Map.insert x t (funs_ m) }

    (_, ss, bs, ds, _) = sepDefs defs

-- }}}

upcast :: Value -> Result
upcast = cataExpr \case
  Ctr c -> Ctr c
  App f x -> App f x

downcast :: Result -> Maybe Value
downcast = cataExprM \case
  Ctr c -> return $ Ctr c
  App f x -> return $ App f x
  _ -> Nothing

-- | Hole fillings
type HF = Map Hole (Term Hole)

-- | Unfilled holes
type UH = Map Hole Constraint

satExample :: MonadReader Mod m => HF -> Result -> Example -> m Bool
satExample hf r ex = case (r, ex) of
  (_, Top) -> return True
  (Apps (Ctr c) xs, Apps (Ctr d) ys) | c == d ->
    and <$> zipWithM (satExample hf) xs ys
  (_, Lam v x) -> do
    r' <- resume hf (App r (upcast v))
    satExample hf r' x
  _ -> return False

type Constraint = [(Scope, Example)]

-- TODO: Can we ignore HF for now?
-- | Unevaluation constraints
type UC = (UH, HF)

-- TODO: are the holefillings here needed?
satConstraint :: MonadReader Mod m => HF -> Term Hole -> Constraint -> m Bool
satConstraint hf e cs = and <$> for cs \(Scope m, ex) -> do
  r <- eval m e >>= resume hf
  satExample hf r ex

-- TODO: are the holefillings and the submap check really needed?
satUneval :: MonadReader Mod m => HF -> UC -> m Bool
satUneval hf (uh, hf0)
  | not $ Map.isSubmapOf hf0 hf = return False
  | otherwise =
    and <$> for (Map.assocs uh) \(h, c) -> satConstraint hf (Hole h) c

-- TODO: do we know that they all have all the same holes?
mergeSolved :: [HF] -> Maybe HF
mergeSolved = sequence . Map.unionsWith go . fmap (fmap Just) where
  go x y = do
    a <- x
    b <- y
    guard (a == b)
    return a

mergeUnsolved :: [UH] -> UH
mergeUnsolved = Map.unionsWith (++)

merge :: [UC] -> Maybe UC
merge cs = let (us, fs) = unzip cs in (mergeUnsolved us,) <$> mergeSolved fs

checkLive :: (MonadPlus m, MonadReader Mod m)
  => Term Hole -> Constraint -> m UH
checkLive e = fmap mergeUnsolved . mapM
  \(Scope m, ex) -> eval m e >>= flip uneval ex

-- TODO: maybe replace Lam by Fix and have Lam be a pattern synonym that sets
-- the recursive argument to Nothing. This makes many things more messy
-- unfortunately.

-- TODO: let uneval just return a 'Map Hole (Map Scope Ex)', or [Example] i.o. Ex
-- TODO: add fuel (probably using a FuelMonad or something, so that we can
-- leave it out as well)
uneval :: (MonadPlus m, MonadReader Mod m) => Result -> Example -> m UH
uneval = curry \case
  -- Top always succeeds.
  (_, Top) -> return mempty
  -- Constructors are handled as opaque functions and their unevaluation is
  -- extensional in the sense that only their arguments are compared.
  (Apps (Ctr c) xs, Lams ys (Apps (Ctr d) zs))
    | c == d, length xs + length ys == length zs
    -> mergeUnsolved <$> zipWithM uneval (xs ++ map upcast ys) zs
  -- Holes are simply added to the environment, with their arguments as inputs.
  -- The arguments should be values in order to obtain correct hole
  -- constraints.
  (Apps (Scoped m (Hole h)) (mapM downcast -> Just vs), ex) ->
    return $ Map.singleton h [(Scope m, lams vs ex)]
  (App (Prj c n) r, ex) -> do
    cs <- arities <$> ask
    let ar = fromMaybe (error "Oh oh") . Map.lookup c $ cs
    uneval r $ apps (Ctr c) (replicate ar Top & ix (n - 1) .~ ex)
  (App (Scoped m (Elim xs)) r, ex) -> do
    (c, e) <- mfold xs
    cs <- arities <$> ask
    let ar = fromMaybe (error "Oh oh") $ Map.lookup c cs
    scrut <- uneval r (apps (Ctr c) (replicate ar Top))
    let prjs = [App (Prj c n) r | n <- [1..ar]]
    e' <- eval m e
    arm <- resume mempty (apps e' prjs) >>= flip uneval ex
    return $ mergeUnsolved [scrut, arm]
  -- Functions should have both input and output, and evaluating their body on
  -- this input should unevaluate onto the output example.
  (Scoped m (Lam a x), Lam v y) ->
    checkLive x [(Scope $ Map.insert a (upcast v) m, y)]
  -- Fixed points additionally add their own definition to the environment.
  (r@(App Fix (Scoped m (Lam f (Indet e)))), ex) ->
    uneval (Scoped (Map.insert f r m) e) ex
  _ -> mzero

-- -- TODO: implement this according to figure 8
-- solve :: UC -> HF
-- solve (uh, hf) = case Map.minViewWithKey uh of
--   Nothing -> hf
--   Just ((h, c), xs) -> _
--
-- fill :: _
-- fill = _ {guess, defer, refine, branch}

-- TODO: note that defer only happens when all examples are top, I think so
-- that other holes can be checked as well.

-- TODO: use Goal like (or rather in place of) hole ctxs. Perhaps make holeCtxs
-- polymorphic in the ctx type.
type Goal = (Map Var Type, Type, Constraint)

allSame :: Eq a => [a] -> Maybe a
allSame = \case
  x:xs | all (==x) xs -> Just x
  _ -> Nothing

-- TODO: maybe fullblown inference is not needed
refine :: (FreshVar m, TCMonad m) => Goal -> m (Term Hole, Map Hole Goal)
refine (goalEnv, goalType, constraints) = do
  let constraints' = filter ((/= Top) . snd) constraints
  ctrs' <- ctrTs <$> ask
  case goalType of
    Arr arg res -> do
      h <- fresh
      f <- fresh
      a <- fresh
      -- TODO: does it make sense to check all the constraints, or should we
      -- just eta-expand immediately, since that is the only reasonable result?
      xs <- failMaybe $ for constraints' \case
        (Scope scope, Lam t u) ->
          let r = App Fix (Scoped scope (Lam f (Lam a (Hole h))))
              scope' = Map.fromList [(f, r), (a, upcast t)]
          in Just (Scope $ scope' <> scope, u)
        _ -> Nothing
      -- TODO: record that f is a recursive call and a is its argument.
      -- Probably using a global variable environment.
      return
        ( App Fix (lams [f, a] (Hole h))
        , Map.singleton h
          ( Map.fromList [(f, goalType), (a, arg)] <> goalEnv
          , res
          , xs
          )
        )
    Apps (Ctr _typeCtr) _typeArgs -> do
      xs <- failMaybe $ for constraints' \case
        (scope, Apps (Ctr ctr) args) -> Just (ctr, (scope, args))
        _ -> Nothing
      let (cs, examples) = unzip xs
      ctr <- failMaybe $ allSame cs
      Poly _bound (Args args ctrType) <- failMaybe $ Map.lookup ctr ctrs'
      th <- unify ctrType goalType
      let examples' = transpose $ sequence <$> examples
      args' <- for args \t -> (, subst th t) <$> fresh
      return
        ( apps (Ctr ctr) (Hole . fst <$> args')
        , Map.fromList $ zipWith (\(h, t) exs ->
          (h, (goalEnv, t, exs))
          ) args' examples'
        )
    _ -> fail "Failed refine"

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

-- Note that example merging might result in more examples. It is probably more
-- useful to just use Ex instead of [Example]
mergeExamples :: [Example] -> Maybe [Example]
mergeExamples = fmap fromEx . fromExamples
