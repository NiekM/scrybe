{-# LANGUAGE UndecidableInstances #-}
module Language.Live where

import Import
import Nondet
import Language.Syntax
import qualified RIO.Map as Map

{-# COMPLETE Scoped, App, Ctr, Fix, Prj #-}
pattern Scoped :: Scope -> Indet -> Expr' r 'Det
pattern Scoped m e = Hole (Annot e m)

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

eval :: Scope -> Term Hole -> Eval Result
eval loc = \case
  Var v -> do
    rs <- view scope
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

evalAssert :: Scope -> Assert -> Eval (Result, Example)
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
      in m & over scope (Map.insert x r)

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

class PartialSemigroup a where
  (<?>) :: a -> a -> Maybe a

-- | Laws
-- - pempty <?> x = Just x
-- - x <?> pempty = Just x
class PartialSemigroup a => PartialMonoid a where
  pempty :: a

instance (Ord k, PartialSemigroup v) => PartialSemigroup (Map k v) where
  (<?>) = mergeMap (<?>)

instance (Ord k, PartialSemigroup v) => PartialMonoid (Map k v) where
  pempty = Map.empty

instance PartialSemigroup (f (g a)) => PartialSemigroup (Compose f g a) where
  Compose x <?> Compose y = Compose <$> x <?> y

instance PartialMonoid (f (g a)) => PartialMonoid (Compose f g a) where
  pempty = Compose pempty

zipMerge :: PartialSemigroup a => [a] -> [a] -> Maybe [a]
zipMerge xs ys
  | length xs == length ys = zipWithM (<?>) xs ys
  | otherwise = Nothing

pfold :: (Foldable f, PartialMonoid a) => f a -> Maybe a
pfold = pfoldMap id

pfoldMap :: (Foldable f, PartialMonoid m) => (a -> m) -> f a -> Maybe m
pfoldMap f = Just pempty & foldr \x r -> r >>= (f x <?>)

pfoldMap' :: (Foldable f, PartialMonoid m) => (a -> m) -> f a -> Maybe m
pfoldMap' f = Just pempty & foldl' \r x -> r >>= (<?> f x)

data Ex
  = ExFun (Map Value Ex)
  | ExCtr Ctr [Ex]
  | ExTop
  deriving (Eq, Ord, Show)

instance PartialSemigroup Ex where
  ExTop <?> ex = Just ex
  ex <?> ExTop = Just ex
  ExFun fs <?> ExFun gs = ExFun <$> fs <?> gs
  ExCtr c xs <?> ExCtr d ys | c == d = ExCtr c <$> zipMerge xs ys
  _ <?> _ = Nothing

instance PartialMonoid Ex where
  pempty = ExTop

instance Pretty Ex where
  pretty = pretty . fromEx

toEx :: Example -> Ex
toEx = \case
  Top -> ExTop
  Apps (Ctr c) xs -> ExCtr c (toEx <$> xs)
  Lam v x -> ExFun (Map.singleton v $ toEx x)
  _ -> error "Incorrect example"

fromExamples :: [Example] -> Maybe Ex
fromExamples = pfoldMap' toEx

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

ctrArity :: MonadReader Env m => Ctr -> m Int
ctrArity c = do
  cs <- view $ env . constructors
  case Map.lookup c cs of
    Nothing -> error $ "Unknown constructor " <> show c
    Just d -> return $ arity d

-- Non-normalizing variant of uneval
un :: Result -> Ex -> Eval (Logic Constraints)
un = curry \case
  -- Top always succeeds.
  (_, ExTop) -> return $ Conjunction []
  -- Constructors are handled as opaque functions and their unevaluation is
  -- extensional in the sense that only their arguments are compared.
  (Apps (Ctr c) xs, ExCtr d ys) -- TODO: should we include Lams here as well?
    | c == d, length xs == length ys
    -> Conjunction <$> zipWithM un xs ys
  -- Holes are simply added to the environment, with their arguments as inputs.
  -- The arguments should be values in order to obtain correct hole
  -- constraints.
  (Apps (Scoped m (Hole h)) (mapM downcast -> Just vs), ex) -> do
    let ex' = foldr (\v -> ExFun . Map.singleton v) ex vs
    return . Pure $ Map.singleton h $ Map.singleton m ex'
  (App (Prj c n) r, ex) -> do
    ar <- ctrArity c
    un r . ExCtr c $ replicate ar ExTop & ix (n - 1) .~ ex
  (App (Scoped m (Elim xs)) r, ex) -> Disjunction <$> for xs \(c, e) -> do
    ar <- ctrArity c
    scrut <- un r . ExCtr c $ replicate ar ExTop
    let prjs = [App (Prj c n) r | n <- [1..ar]]
    e' <- eval m e
    arm <- resume mempty (apps e' prjs) >>= flip un ex
    return $ Conjunction [scrut, arm]
  -- Functions should have both input and output, and evaluating their body on
  -- this input should unevaluate onto the output example.
  (Scoped m (Lam a e), ExFun xs) ->
    Conjunction <$> for (Map.assocs xs) \(v, ex) -> do
      r <- eval (Map.insert a (upcast v) m) e
      un r ex
  -- Fixed points additionally add their own definition to the environment.
  (r@(App Fix (Scoped m (Lam f (Indet e)))), ex) ->
    un (Scoped (Map.insert f r m) e) ex
  _ -> return $ Disjunction []

reLive :: Map Hole (Term Hole) -> Term Hole -> Constraint ->
  Eval (Logic Constraints)
reLive hf e cs = Conjunction <$> for (Map.assocs cs) \(m, ex) -> do
  r <- eval m e >>= resume hf
  un r ex

re :: Map Hole (Term Hole) -> Logic Constraints -> Eval (Logic Constraints)
re hf = fmap join . traverse \old -> do
  -- Update the old constraints by resuming their local scopes and then
  -- merging them.
  foo <- mapM (mergeFromAssocs (<?>)) <$> for old \c ->
    forOf (each . _1) (Map.assocs c) $ traverseOf each (resume hf)
  case foo of
    Nothing -> return $ Disjunction []
    Just upd -> do
      -- Compute the new constraints that arise from filling each hole.
      new <- sequence . toList $ Map.intersectionWith (reLive hf) hf upd
      -- Combine the new constraints with the updated constraints, minus the
      -- filled holes.
      return . fmap (Map.\\ hf) . Conjunction $ Pure upd : new

mergeConstraints :: MonadPlus m => [Constraints] -> m Constraints
mergeConstraints = mfold . mergeMaps (<?>)

dnf :: Logic a -> [[a]]
dnf = \case
  Pure a -> [[a]]
  Conjunction xs -> concat <$> mapM dnf xs
  Disjunction xs -> concat $ dnf <$> xs

-- TODO: replace fuel burning with a simple check in Synthesis to see if the
-- normalized constraint is too large. Then move all calls to mergeConstraints
-- to Synthesis, as well as remove Uneval altogether.
uneval :: Result -> Ex -> Uneval Constraints
uneval r ex = do
  l <- liftEval $ un r ex
  for_ (dnf l) $ const burnFuel
  xs <- mfold $ dnf l
  mergeConstraints xs

-- TODO: fix tree_size.hs not working with this version
resumeUneval :: Map Hole (Term Hole) -> Constraints -> Uneval Constraints
resumeUneval hf old = do
  l <- liftEval (re hf $ Pure old)
  for_ (dnf l) $ const burnFuel
  xs <- mfold $ dnf l
  mergeConstraints xs

unAssert :: Scope -> Assert -> Eval (Logic Constraints)
unAssert m (MkAssert e ex) = do
  r <- eval m e
  un r $ toEx ex

unevalAssert :: Scope -> Assert -> Uneval Constraints
unevalAssert m a = do
  l <- liftEval (unAssert m a)
  xs <- mfold $ dnf l
  mergeConstraints xs

-- }}}
