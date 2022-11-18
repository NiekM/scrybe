{-# LANGUAGE TemplateHaskell #-}

module Synthesis (synth, runSynth) where

import Import
import Options (SynOptions(..), synPropagate)
import Utils.Weighted
import Language
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
liftInfer = mapRWST mfold . zoom freshFree

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

synth :: Defs Unit -> Synth Fillings
synth d = init d >> go where
  go :: Synth Fillings
  go = do
    st <- get
    if final st
      then use fillings
      else step mempty >> go

final :: SynState -> Bool
final = null . view contexts

init :: Defs Unit -> Synth (Term Hole)
init defs = do
  assign examples $ asserts defs

  incl     <- liftInfer $ checkIncluded defs
  expr     <- liftInfer $ checkBindings defs
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
  g@(Goal ctx t) <- mfold . Map.lookup h =<< use contexts

  (fun, p)  <- choices g
  Args ts u <- zoom freshFree $ instantiateFresh p
  th        <- mfold $ unify u t

  let expr  =  apps fun (Hole . Goal ctx <$> ts)
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
checkBindings :: Defs Unit -> Infer (Term Goal)
checkBindings defs = do
  (_, a) <- infer mempty expr
  case a of
    x@(Lets _ (Hole (Goal cs _))) -> do
      let funs = cs <&> \(Poly _ t) -> t
      th <- failMaybe . unifies $ Map.intersectionWith (,) funs sigs
      return $ over holes
        ( over goalType freezeAll
        . over goalCtx (freezeUnbound . instantiate th <$>)
        . subst th
        ) x
    _ -> error "Impossible"
  where
    sigs = Map.fromList $ signatures defs <&>
      \(MkSignature a (Poly _ t)) -> (a, t)
    expr = Lets (bindings defs <&> \(MkBinding a x) -> (a, x)) (Hole Unit)

updateConstraints :: [[Constraints]] -> Synth ()
updateConstraints xs = do
  let zs = mapMaybe mergeConstraints xs
  let ys = nubOrd zs
  guard . not . null $ ys
  assign constraints $ Disjunction . fmap Pure $ ys

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

choices :: Goal -> Synth (Term Goal, Poly)
choices (Goal ctx t) = do
  ds <- view envDatatypes
  fs <- use included
  mfold $ join
    [ case t of
        Apps (Ctr d) _ | Just (as, cs) <- Map.lookup d ds -> do
          (c, ts) <- cs
          return (Ctr c, Poly as . arrs $ ts ++ [apps (Ctr d) (Var <$> as)])
        _ -> []
    , first Var <$> Map.assocs ctx
    , first Var <$> fs
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
