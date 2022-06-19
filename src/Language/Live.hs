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
    maybe undefined return $ lookup v (loc <> rs)
  App f x -> do
    f' <- eval loc f
    x' <- eval loc x
    evalApp f' x'
  Let a x y -> do
    x' <- eval loc x
    eval ((a, x') : loc) y
  Ctr c -> return $ Ctr c
  Fix -> return Fix
  -- Indeterminate results
  Indet i -> return $ Scoped loc i

evalApp :: Result -> Result -> Eval Result
evalApp f x = case f of
  App Fix (Scoped m (Lam g (Indet e))) ->
    evalApp (Scoped ((g, f) : m) e) x
  Scoped m (Lam a y) -> eval ((a, x) : m) y
  Scoped m (Elim xs)
    | Apps (Ctr c) as <- x
    , Just (Lams bs y) <- lookup c xs
    -> eval (zip bs as <> m) y
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
    m' <- traverseOf (each . _2) (resume hf) m
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
      in m & over scope ((x, r):)

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

mergeConstraints :: MonadPlus m => [Constraints] -> m Constraints
mergeConstraints = mfold . mergeMaps (<?>)

live :: Term Hole -> Constraint -> Uneval Constraints
live e cs =
  mergeConstraints =<< for (Map.assocs cs) \(m, ex) -> do
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
    return $ Map.singleton h $ Map.singleton m $ toEx $ lams vs ex
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
    live x $ Map.singleton ((a, upcast v) : m) (toEx y)
  -- Fixed points additionally add their own definition to the environment.
  (r@(App Fix (Scoped m (Lam f (Indet e)))), ex) ->
    uneval (Scoped ((f, r) : m) e) ex
  _ -> mzero

-- TODO: Perhaps throw away normal checkLive in favor of this
resumeLive :: Map Hole (Term Hole) -> Term Hole -> Constraint ->
  Uneval Constraints
resumeLive hf e cs =
  mergeConstraints =<< for (Map.assocs cs) \(m, ex) -> do
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
  updated <- mfold . mapM (mergeFromAssocs (<?>)) =<< for old \c ->
    forOf (each . _1) (Map.assocs c) $ traverseOf (each . _2) (liftEval . resume hf)
  -- Compute the new constraints that arise from filling each hole.
  -- TODO: check that this line is correct now. Can we optimize it?
  new <- sequence . toList $ Map.intersectionWith (resumeLive hf) hf updated
  -- Combine the new constraints with the updated constraints, minus the filled
  -- holes.
  mergeConstraints . fmap (Map.\\ hf) $ updated : new

unevalAssert :: Scope -> Assert -> Uneval Constraints
unevalAssert m (MkAssert e ex) = do
  r <- liftEval $ eval m e
  uneval r ex

-- }}}

-- Deterministic Constraints
data Tree m v
  = Node v
  | Branch (m (Tree m v))
  deriving (Functor, Foldable, Traversable)

deriving instance (Eq v, Eq (m (Tree m v))) => Eq (Tree m v)
deriving instance (Ord v, Ord (m (Tree m v))) => Ord (Tree m v)
deriving instance (Show v, Show (m (Tree m v))) => Show (Tree m v)
deriving instance (Read v, Read (m (Tree m v))) => Read (Tree m v)

instance (Pretty (m (Tree m v)), Pretty v) => Pretty (Tree m v) where
  pretty = \case
    Node a -> pretty a
    Branch xs -> pretty xs

withKeys :: Tree (Map k) v -> Tree (Map k) ([k], v)
withKeys = \case
  Node a -> Node ([], a)
  Branch xs -> Branch $ Map.mapWithKey (\k -> (first (k:) <$>) . withKeys) xs

type Map2 k l = Compose (Map k) (Map l)

withKeys2 :: Tree (Map2 k l) v -> Tree (Map2 k l) ([(k, l)], v)
withKeys2 = \case
  Node a -> Node ([], a)
  Branch (Compose xs) -> Branch . Compose $ xs &
    Map.mapWithKey \k -> Map.mapWithKey \l -> (first ((k,l):) <$>) . withKeys2

treeGrow :: [k] -> Tree (Map k) v -> Tree (Map k) v
treeGrow ks t = foldr (\k -> Branch . Map.singleton k) t ks

treeGrow2 :: [(k, l)] -> Tree (Map2 k l) v -> Tree (Map2 k l) v
treeGrow2 ks t = foldr
  (\(k, l) -> Branch . Compose . Map.singleton k . Map.singleton l) t ks

instance (Traversable m, PartialSemigroup (m (Tree m v)), PartialSemigroup v)
  => PartialSemigroup (Tree m v) where
  Node x <?> Node y = Node <$> x <?> y
  Node x <?> Branch ys
    | null ys = Just $ Node x
    | otherwise = mapM (x <?>) (Branch ys)
  Branch xs <?> Node y
    | null xs = Just $ Node y
    | otherwise = mapM (<?> y) (Branch xs)
  Branch xs <?> Branch ys = Branch <$> xs <?> ys

instance (Traversable m, PartialMonoid (m (Tree m v)), PartialSemigroup v)
  => PartialMonoid (Tree m v) where
  pempty = Branch pempty

fromScope :: v -> [(k, l)] -> Tree (Map2 k l) v
fromScope y = Node y & foldr \(a, b) ->
  Branch . Compose . Map.singleton a . Map.singleton b

-- TODO: perhaps for the Hole & Ctr we also want two composed maps
type Cs = Tree (Map (Hole, Ctr)) (Tree (Map2 Var Result) (Map Hole Ex))

__mergeAll :: [Maybe Cs] -> Maybe Cs
__mergeAll = sequence >=> pfold

__mergeAlt :: [Maybe Cs] -> Maybe Cs
__mergeAlt cs = case catMaybes cs of
  [] -> Nothing
  ds -> pfold ds

__uneval :: Result -> Ex -> Eval (Maybe Cs)
__uneval = curry \case
  -- Top always succeeds.
  (_, ExTop) -> return . Just $ Branch mempty
  -- Constructors are handled as opaque functions and their unevaluation is
  -- extensional in the sense that only their arguments are compared.
  (Apps (Ctr c) xs, ExCtr d ys) -- TODO: should we include Lams here as well?
    | c == d, length xs == length ys
    -> __mergeAll <$> zipWithM __uneval xs ys
  -- Holes are simply added to the environment, with their arguments as inputs.
  -- The arguments should be values in order to obtain correct hole
  -- constraints.
  (Apps (Scoped m (Hole h)) (mapM downcast -> Just vs), ex) -> do
    let ex' = foldr (\v -> ExFun . Map.singleton v) ex vs
    return . Just . Node $ fromScope (Map.singleton h ex') (reverse m)
  (App (Prj c n) r, ex) -> do
    cs <- view $ env . constructors -- TODO: something like view constructors
    let ar = arity . fromMaybe (error "Oh oh") $ Map.lookup c cs
    __uneval r . ExCtr c $ replicate ar ExTop & ix (n - 1) .~ ex
  (App (Scoped m (Elim xs)) r, ex) -> do
    cs <- view $ env . constructors
    -- TODO: computing the blocking hole here is incorrect for e.g.
    -- `plus {0} (plus {1} {2}) <== 1`, as {2} is always considered the
    -- blocking hole.
    let b = fromMaybe undefined $ blocking r
    __mergeAlt <$> for xs \(c, e) -> do
      let ar = arity . fromMaybe (error "Oh oh") $ Map.lookup c cs
      scrut <- __uneval r . ExCtr c $ replicate ar ExTop
      let prjs = [App (Prj c n) r | n <- [1..ar]]
      e' <- eval m e
      arm <- resume mempty (apps e' prjs) >>= flip __uneval ex
      return . fmap (Branch . Map.singleton (b, c)) . __mergeAll $ [scrut, arm]
  -- Functions should have both input and output, and evaluating their body on
  -- this input should unevaluate onto the output example.
  (Scoped m (Lam a e), ExFun xs) ->
    __mergeAll <$> for (Map.assocs xs) \(v, ex) -> do
      r <- eval ((a, upcast v) : m) e
      __uneval r ex
  -- Fixed points additionally add their own definition to the environment.
  (r@(App Fix (Scoped m (Lam f (Indet e)))), ex) ->
    __uneval (Scoped ((f, r) : m) e) ex
  _ -> return Nothing

mergeTree :: Tree (Map2 Var Result) [Maybe Cs] -> Maybe Cs
mergeTree = \case
  Node a -> __mergeAll a
  Branch (Compose xs) ->
    -- NOTE: differing variables should be combined (__mergeAll)
    -- whereas differing values should be considered separately (__mergeAlt)
    __mergeAll . toList $ fmap (__mergeAlt . toList . fmap mergeTree) xs

-- TODO: test or prove that this is the correct unevaluation equivalent of
-- resumption, i.e. that resuming and then unevaluation is equivalent to
-- unevaluating and then "un-resuming".
__resumeUneval :: Map Hole (Term Hole) -> Cs -> Eval (Maybe Cs)
__resumeUneval hf old = do
  __mergeAlt . toList <$> for (withKeys old) \(t, xs) -> do
    let t' = filter ((`Map.notMember` hf) . fst) t
    mergeTree <$> for (withKeys2 xs) \(m, ys) -> do
      m' <- traverseOf (each . _2) (resume hf) m
      for (Map.assocs ys) \(h, ex) -> do
        fmap (treeGrow t') <$> case Map.lookup h hf of
          Nothing ->
            return . Just . Node . treeGrow2 m' . Node $ Map.singleton h ex
          Just e -> do
            r <- eval m' e >>= resume hf
            __uneval r ex
