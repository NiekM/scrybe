module Language.Live where

import Import hiding (reverse)
import Language.Syntax
import Language.Type
import RIO.List
import qualified RIO.Map as Map

{-# COMPLETE Scoped, App, Ctr, Fix #-}
pattern Scoped :: Map Var Result -> Indet -> Expr' r 'Det
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

{-# COMPLETE Indet, Var, App, Ctr, Fix, Let #-}
pattern Indet :: Indet -> Term Hole
pattern Indet i <- (indet -> Just i)

evalApp :: Result -> Result -> Result
evalApp f x = case f of
  App Fix (Scoped m (Lam g (Indet e))) ->
    evalApp (Scoped (Map.insert g f m) e) x
  Scoped m (Lam a y) -> eval (Map.insert a x m) y
  Scoped m (Elim xs)
    | Apps (Ctr c) as <- x
    , Just (Lams bs y) <- lookup c xs
    -> eval (Map.fromList (zip bs as) <> m) y
  r -> App r x

eval :: Map Var Result -> Term Hole -> Result
eval m = \case
  Var v -> fromMaybe undefined $ Map.lookup v m
  App f x -> evalApp (eval m f) (eval m x)
  Ctr c -> Ctr c
  Fix -> Fix
  Let _a _x _y -> undefined
  -- Indeterminate results
  Indet i -> Scoped m i

-- TODO: Note that resumption loops forever if the hole fillings are (mutually)
-- recursive. An alternative would be to only allow resumption of one hole at a
-- time.
resume :: Map Hole (Term Hole) -> Result -> Result
resume hf = cataExpr \case
  App f x -> evalApp f x
  Ctr c -> Ctr c
  Fix -> Fix
  Prj c n -> Prj c n
  -- Indeterminate results
  Scoped m (Hole h)
    | Just x <- Map.lookup h hf
    , x /= Hole h -> resume hf (eval m x)
  Scoped m e -> Scoped (resume hf <$> m) e

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

satExample :: HF -> Result -> Example -> Bool
satExample hf r ex = case (r, ex) of
  (_, Top) -> True
  (Apps (Ctr c) xs, Apps (Ctr d) ys) ->
    c == d && and (zipWith (satExample hf) xs ys)
  (_, Lam v x) -> satExample hf (resume hf (App r (upcast v))) x
  _ -> False

type Constraint = [(Scope, Example)]

-- TODO: Can we ignore HF for now?
-- | Unevaluation constraints
type UC = (UH, HF)

-- TODO: are the holefillings here needed?
satConstraint :: HF -> Term Hole -> Constraint -> Bool
satConstraint hf e = all \(m, ex) ->
  satExample hf (resume hf $ eval (unScope m) e) ex

-- TODO: are the holefillings and the submap check really needed?
satUneval :: HF -> UC -> Bool
satUneval hf (uh, hf0) = Map.isSubmapOf hf0 hf
  && all (\(h, c) -> satConstraint hf (Hole h) c) (Map.assocs uh)

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

checkLive :: Map Ctr Int -> Term Hole -> Constraint -> [UC]
checkLive cs e = mapMaybe merge . mapM \(Scope env, ex) ->
  uneval cs (eval env e) ex

-- TODO: maybe replace Lam by Fix and have Lam be a pattern synonym that sets
-- the recursive argument to Nothing. This makes many things more messy
-- unfortunately.

-- TODO: add fuel (probably using a FuelMonad or something, so that we can
-- leave it out as well)
uneval :: Map Ctr Int -> Result -> Example -> [UC]
uneval cs = curry \case
  -- Top always succeeds.
  (_, Top) -> [mempty]
  -- Constructors are handled as opaque functions and their unevaluation is
  -- extensional in the sense that only their arguments are compared.
  (Apps (Ctr c) xs, Lams ys (Apps (Ctr d) zs))
    | c == d, length xs + length ys == length zs
    -> mapMaybe merge $ zipWithM (uneval cs) (xs ++ map upcast ys) zs
  -- Holes are simply added to the environment, with their arguments as inputs.
  -- The arguments should be values in order to obtain correct hole
  -- constraints.
  (Apps (Scoped m (Hole h)) (mapM downcast -> Just vs), ex) ->
    [(Map.singleton h [(Scope m, lams vs ex)], mempty)]
  (App (Prj c n) r, ex) ->
    let arity = fromMaybe (error "Oh oh") $ Map.lookup c cs
    in uneval cs r $ apps (Ctr c) (replicate arity Top & ix (n - 1) .~ ex)
  (App (Scoped m (Elim xs)) r, ex) -> do
    (c, e) <- xs
    let arity = fromMaybe (error "Oh oh") $ Map.lookup c cs
    scrut <- uneval cs r (apps (Ctr c) (replicate arity Top))
    let prjs = [App (Prj c n) r | n <- [1..arity]]
    arm <- uneval cs (resume mempty (apps (eval m e) prjs)) ex
    maybeToList $ merge [scrut, arm]
  -- Functions should have both input and output, and evaluating their body on
  -- this input should unevaluate onto the output example.
  (Scoped m (Lam a x), Lam v y) ->
    checkLive cs x [(Scope $ Map.insert a (upcast v) m, y)]
  -- Fixed points additionally add their own definition to the environment.
  -- TODO: Does this work/make sense?
  (r@(App Fix (Scoped m (Lam f (Indet e)))), ex) ->
    uneval cs (Scoped (Map.insert f r m) e) ex
  _ -> []

-- -- TODO: implement this according to figure 8
-- solve :: UC -> HF
-- solve (uh, hf) = case Map.minViewWithKey uh of
--   Nothing -> hf
--   Just ((h, c), xs) -> _
--
-- fill :: _
-- fill = _

type Goal = (Map Var Type, Type, Constraint)

allSame :: Eq a => [a] -> Maybe a
allSame = \case
  x:xs | all (==x) xs -> Just x
  _ -> Nothing

-- TODO: maybe fullblown inference is not needed
refine :: Infer s m => Goal -> m (Term Hole, Map Hole Goal)
refine (goalEnv, goalType, constraints) = do
  let constraints' = filter ((/= Top) . snd) constraints
  ctrs' <- ctrs <$> ask -- TODO: use datatypes instead?
  case goalType of
    Arr arg res -> do
      h <- fresh
      f <- varId <$> fresh
      a <- varId <$> fresh
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
      Poly _bound (Args args ctrType) <- failMaybe $ lookup ctr ctrs'
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

-- TODO: this doesn't really work for higher-order data structures such as
-- Maybe (Nat -> Nat), but maybe we shouldn't focus on those. Do we still have
-- this problem if the expression is in eta-long form?
exampleMap :: Example -> Map [Value] Example
exampleMap = uncurry Map.singleton . unLams

mergeEx :: Example -> Example -> Maybe Example
mergeEx = curry \case
  (ex, Top) -> Just ex
  (Top, ex) -> Just ex
  (Ctr c, Ctr d) | c == d -> Just $ Ctr c
  (App f x, App g y) -> App <$> mergeEx f g <*> mergeEx x y
  _ -> Nothing

mergeExamples :: [Example] -> Maybe (Map [Value] Example)
mergeExamples = sequence
  . Map.unionsWith ((join .) . liftM2 mergeEx)
  . fmap (fmap Just . exampleMap)
