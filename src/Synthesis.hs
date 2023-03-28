{-# LANGUAGE TemplateHaskell, MultiWayIf #-}

module Synthesis (synth, runSynth, Fillings) where

import Import
import Data.List (unzip)
import Options (SynOptions(..), synPropagate)
import Utils.Weighted
import Language
import Constraint
import qualified RIO.Map as Map

-- Synthesis Monad {{{

type Fillings = Map Hole (Term Hole)
type Forbidden = Map Hole (Term Unit)

data SynState = SynState
  { _options     :: SynOptions
  , _contexts    :: Map Hole Goal
  , _constraints :: Logic Constraints
  , _examples    :: [Assert]
  , _included    :: [(Var, Poly)]
  , _forbidden   :: [Forbidden]
  , _mainScope   :: Scope
  , _fillings    :: Fillings
  -- Fresh variables
  , _freshHole   :: Fresh Hole
  , _freshFree   :: Fresh Free
  , _freshVar    :: Fresh Var
  -- Heuristics
  , _lamCount    :: Sum Int
  }

makeLenses ''SynState

mkSynState :: SynOptions -> SynState
mkSynState opts = SynState opts mempty (Disjunction []) mempty mempty mempty
  mempty mempty mempty mempty mempty mempty

type Nondet = Search Dist
type Synth = RWST Env () SynState Nondet

runSynth :: SynOptions -> Env -> Synth a -> Nondet a
runSynth opts m x = view _1 <$> runRWST x m (mkSynState opts)

liftInfer :: Infer a -> Synth a
liftInfer = mapRWST (maybe (error "Type error") return) . zoom freshFree

liftUneval :: Int -> Uneval a -> Synth (Maybe a)
liftUneval fuel x = ask <&> \e -> view _1 <$> runRWST x e fuel

-- }}}

-- Options {{{

unevalFuel :: Int
unevalFuel = 32

maxDisjunctions :: Int
maxDisjunctions = 32

noFuelWeight :: Dist
noFuelWeight = 1

maxDisjunctionWeight :: Dist
maxDisjunctionWeight = 4

-- }}}

synth :: Defs Unit -> Synth (Term Hole, Fillings)
synth d = do
  e  <- init d
  hf <- go
  return (e, hf)
  where
    go :: Synth Fillings
    go = do
      st <- get
      if final st
        then use fillings
        else step mempty >> go

final :: SynState -> Bool
-- final = _noHoles
final = noConstraints

noConstraints :: SynState -> Bool
noConstraints s = case view constraints s of
  Disjunction [Pure x] | null x -> True
  _ -> False

_noHoles :: SynState -> Bool
_noHoles = null . view contexts

init :: Defs Unit -> Synth (Term Hole)
init defs = do
  assign examples $ asserts defs

  incl         <- liftInfer $ checkIncluded defs
  (sigs, expr) <- liftInfer $ checkBindings defs
  liftInfer $ checkAsserts (Map.fromList incl <> sigs) defs
  expanded <- zoom freshVar  $ expand  expr
  (e, ctx) <- zoom freshHole $ extract expanded

  assign included incl
  assign contexts ctx

  liftEval (eval mempty e) >>= \case
    Scoped m (Hole h) -> do
      assign mainScope m
      modifying contexts $ Map.delete h
      -- NOTE: since sketching is not the main focus in scrybe, we assume here
      -- that sketching succeeds.
      liftUneval 1000 (unevalAssert m `mapM` asserts defs) >>= \case
        Nothing -> fail "Out of fuel"
        Just xs -> updateConstraints . dnf . Conjunction $ xs
      return e
    _ -> error "Should never happen"

step :: Fillings -> Synth ()
step hf = do

  (m, hole) <- findBlocking
  expr      <- refinements hole

  -- Update hole fillings.
  let new = Map.singleton hole expr
  use mainScope
    >>= mapM (liftEval . resume new)
    >>= assign mainScope
  modifying fillings (<> new)

  scrHole <- scrutinizedHole <$> liftEval do
    eval m expr >>= resume new

  cs <- use constraints
  ctxs <- use contexts
  propagate <- use $ options . synPropagate

  let hf' = hf <> new

  case scrHole of
    -- We eagerly fill scrutinized holes by recursively calling step.
    Just _ -> step hf'
    Nothing
      -- If propagation is turned off, we recursively call step unless there
      -- are no holes left.
      | not propagate && not (Map.null ctxs) -> step hf'
      -- ... otherwise we propagate examples.
      | otherwise -> liftUneval unevalFuel (resumeUneval hf' cs) >>= \case
        -- If example propagations runs out of fuel, we recursively call step,
        -- with some weight.
        Nothing -> do
          weigh noFuelWeight
          step hf'
        Just xs
          -- If example propagation returns too many disjunctions, we make the
          -- assumption that it is more efficient too recursively call step, as
          -- well as adding some weight.
          | length disjunctions > maxDisjunctions -> do
            weigh maxDisjunctionWeight
            step hf'
          -- If example propagation succeeds with a reasonable amount of
          -- disjunctions we update the constraints.
          | otherwise -> updateConstraints disjunctions
          where disjunctions = dnf xs

refinements :: Hole -> Synth (Term Hole)
refinements h = do
  g@(Goal ctx t) <- maybe (error "Missing goal") return
    . Map.lookup h =<< use contexts

  (fun, p)  <- choices g
  Args ts u <- zoom freshFree $ instantiateFresh p
  th        <- mfold $ unify u t

  let
    args = Hole . Goal ctx <$> ts
    expr = case fun of
      Left f -> apps (Var f) args
      Right c -> Ctr c args
  expanded  <- zoom freshVar  $ expand expr
  (e, ctx') <- zoom freshHole $ extract expanded

  zoom contexts do
    modify $ Map.delete h
    modify $ Map.union ctx'
    modify (subst th <$>)

  -- Weigh 1 for each new hole
  weigh @Dist $ fromIntegral $ length ts
  -- Each new lambda weighs more
  for_ ts \(Args as _) -> for_ as $ const addLam

  updateForbidden h e

  return e

checkIncluded :: Defs Unit -> Infer [(Var, Poly)]
checkIncluded defs = do
  let fs = [x | Include xs <- pragmas defs, x <- toList xs]
  gs <- view envFunctions
  for fs \(f, s) -> do
    case Map.lookup f gs of
      Nothing -> error $ "Unknown function " <> show f
      Just p@(Poly _ t) -> (f,) <$> case s of
        Nothing -> return p
        Just q -> do
          Poly _ u <- refresh q
          case unify u t of
            Nothing ->
              error $ "Included function " <> show f <> " has wrong type"
            Just th -> return $ poly (subst th u)

-- Compute a sketch from a set of bindings with a single hole at the end,
-- then type check
checkBindings :: Defs Unit -> Infer (Map Var Poly, Term Goal)
checkBindings defs = do
  (_, a) <- infer mempty expr
  case a of
    x@(Lets _ (Hole (Goal cs _))) -> do
      let funs = cs <&> \(Poly _ t) -> t
      th <- failMaybe . unifies $ Map.intersectionWith (,) funs sigs
      return  . (poly <$> sigs,) $ over holes
        ( over goalType freezeAll
        . over goalCtx (freezeUnbound . instantiate th <$>)
        . subst th
        ) x
    _ -> error "Impossible"
  where
    sigs = Map.fromList $ signatures defs <&>
      \(MkSignature a (Poly _ t)) -> (a, t)
    expr = Lets (bindings defs <&> \(MkBinding a x) -> (a, x)) (Hole Unit)

checkAsserts :: Map Var Poly -> Defs Unit -> Infer ()
checkAsserts env defs = for_ (asserts defs)
  \(MkAssert expr (Lams vals ex)) -> do
    Args ts t <- fst <$> infer env expr
    guard $ length vals == length ts
    let
      res = ex & cataExpr \case
        Ctr c xs -> Ctr c xs
        Hole h -> Hole h
        App f x -> App f x
        Lam _ _ -> error "Should never happen"
    checks env $ (res, t) : zip (upcast <$> vals) ts

type Simple = RWST () [(Hole, Cstr)] (Fresh Uni) Maybe

recover :: [(Hole, cstr)] -> Map Hole [cstr]
recover = Map.unionsWith (++) . fmap \(h, c) -> Map.singleton h [c]

-- simplifyResult :: Result -> Simple Value
-- simplifyResult = \case
--   Ctr c xs -> Ctr c <$> traverse simplifyResult xs
--   -- App f x -> App <$> simplifyResult f <*> simplifyResult x
--   Scoped m (Hole h) -> do
--     MkUni t <- fresh
--     (as, xs) <- unzip <$> simplifyScope m
--     tell [(h, Cstr as [InOut xs (Ctr (MkCtr t) [])])]
--     return $ Ctr (MkCtr t) []
--   r -> error $ "Should not happen: " ++ show (pretty r)

simplifyScope :: Scope -> Simple [(Var, Value)]
simplifyScope = fmap Map.assocs . traverse (maybe (fail "") return . downcast)

simplifyConstraint :: Constraint -> Simple (Maybe [Cstr])
simplifyConstraint = fmap (fmap (fmap go . flatten))
  . traverse (traverseOf (each . _1) simplifyScope . Map.assocs)
  . traverse (traverse downcast . fromEx)
  where
    go (unzip -> (as, is), o) = Cstr as [InOut is o]
    flatten = concat . traverseOf (each . _2) id

joinCstrs :: [Cstr] -> Cstr
joinCstrs [] = error "Oopsie woopsie"
joinCstrs (Cstr as xs : cs) = Cstr as (xs ++ concatMap io cs)
  where io (Cstr _ ys) = ys

updateConstraints :: [[Constraints]] -> Synth ()
updateConstraints xs = do
  let parametric = True
  let ys = nubOrd (mapMaybe mergeConstraints xs)
  if
    | null xs -> fail "No possible constraints"

    | parametric -> do
      let
        cstrs = traverse (traverse simplifyConstraint) ys
        ws = traverseOf (_1 . each . each) id =<< evalRWST cstrs () 0
        cs = ws <&> \(x, y) -> x <&> fmap joinCstrs . Map.unionWith (++) (recover y)
        norm = cs <&> fmap (fmap normCstr)
        checked = norm <&> fmap (fmap checkNorm)
        is = case checked of
          Nothing -> ys
          Just e -> map fst . filter (isJust . snd) . zip ys $ sequence <$> e
        n = length ys - length is
      when (n > 0) do traceShowM n
      guard . not $ null is
      assign constraints $ Disjunction . fmap Pure $ is

    | otherwise -> assign constraints $ Disjunction . fmap Pure $ ys

findBlocking :: Synth (Scope, Hole)
findBlocking = do
  s <- use mainScope
  rs <- use examples >>= mapM (liftEval . evalAssert s)
  -- TODO: if there are multiple blocking holes, we could compute hole
  -- dependencies and base the order on that
  mfold $ foldr (<|>) Nothing (blocking . fst <$> rs)

addLam :: Synth ()
addLam = do
  Sum n <- use lamCount
  weigh @Dist $ fromIntegral n
  modifying lamCount (+1)

choices :: Goal -> Synth (Either Var Ctr, Poly)
choices (Goal ctx t) = do
  ds <- view envDatatypes
  fs <- use included
  mfold $ join
    [ case t of
        Ctr d _ | Just (as, cs) <- Map.lookup d ds -> do
          (c, ts) <- cs
          return (Right c, Poly as . arrs $ ts ++ [Ctr d (Var <$> as)])
        _ -> []
    , first Left <$> Map.assocs ctx
    , first Left <$> fs
    ]

checkForbidden :: Hole -> Term Hole -> Forbidden -> Possibly Forbidden
checkForbidden h e old = case Map.lookup h old of
  Nothing -> Perhaps old
  Just x -> case match e x of
    Nothing -> No
    Just new
      | null upd -> Yes
      | otherwise -> Perhaps upd
      where upd = Map.delete h old <> new

updateForbidden :: Hole -> Term Hole -> Synth ()
updateForbidden h e = do
  old <- use forbidden
  upd <- mfold . possibly $ checkForbidden h e <$> old
  new <- mapMaybe (match e) . view envForbidden <$> ask
  assign forbidden $ upd <> new
