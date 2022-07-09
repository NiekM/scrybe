{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}

module Synthesis where

import Import
import Language
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import Control.Monad.Heap
import Control.Monad.State
import Data.Monus.Dist

-- TODO: maybe we also want to keep track of a list of possible refinements
-- e.g. [Term Unit]
data SynState = SynState
  { _contexts    :: Map Hole HoleCtx
  , _constraints :: Logic Constraints
  , _fillings    :: Map Hole (Term Hole)
  , _freshSt     :: FreshState
  , _mainScope   :: Scope
  , _examples    :: [Assert]
  , _weights     :: Map Var Int
  }

makeLenses ''SynState

emptySynState :: SynState
emptySynState =
  SynState mempty (Disjunction []) mempty mkFreshState mempty [] mempty

runSynth :: Env -> Synth a -> Heap Dist a
runSynth m x = fst <$> runReaderT (runStateT x emptySynState) m

instance HasFreshState SynState where
  freshState = freshSt

type Synth = StateT SynState (ReaderT Env (Heap Dist))

instance LiftEval Synth where
  liftEval x = runIdentity . runReaderT x <$> view env

-- | Retrieve the context of a hole.
getCtx :: Hole -> Synth HoleCtx
getCtx h = use contexts >>=
  maybe (fail $ "Missing holeCtx for hole " <> show h) return . Map.lookup h

instance ExpandHole Hole Synth where
  expandHole h = do
    (xs, ctx') <- getCtx h >>= expandHole
    modifying contexts $ Map.insert h ctx'
    return (xs, h)

liftUneval :: Int -> Uneval a -> Synth (Maybe a)
liftUneval fuel x = ask <&> \e -> view _1 <$> runRWST x e fuel

-- TODO: figure out how to deal with diverging unevaluation, such as that of
-- 'foldList {} (\x r -> r) {}' onto some examples, like '\[] -> 0', or for
-- example 'mult 0 {0} <= 0'.
-- NOTE: it seems to keep trying to merge more and more elaborate examples
-- This seems to be a very specific example where unevaluation diverges, which
-- is usually not encountered during synthesis, but only in these specific
-- circumstances?

-- TODO: make sure that local type unifications don't leak to other places. For
-- example, when synthesizing 'map', we might define 'map_succ' (a helper
-- function for assertions) as 'map Succ', but this instantiates the type
-- variables of 'map' to 'Nat', which is incorrect. Not only does this lead to
-- the type incorrect synthesis of 'map' specialized to 'Succ', but if we had
-- another helper function such as 'map_unit = map (const Unit)', the resulting
-- unification error would stop synthesis from occuring altogether.

-- TODO: implement a function that reads and type checks the program correctly,
-- without leaking type information between functions. To what extend do we
-- consider the toplevel functions part of the global/local environment? It is
-- not great to mix the current local scope (based on unevaluation constraints
-- from the assertions) with the semi-local scope of toplevel functions (which
-- might still contain holes and are not necessarily globally available in
-- every hole context). The resulting function should be somewhat similar to
-- the fromDefs function in Language.Live.hs, in that it folds over all the
-- toplevel definitions.
-- ALTERNATIVELY: implement full let polymorphism.

-- TODO: perhaps Nondet should be a wrapper around Heap providing this
-- MonadFail instance.
instance MonadFail (Heap w) where
  fail _ = mzero

init :: Defs Unit -> Synth (Term Hole)
init defs = do
  let ws = concat $ imports defs <&> \(MkImport _ xs) -> fromMaybe [] xs
  -- TODO: determine weights based on something
  assign weights $ Map.fromList ((,1) <$> ws)
  let addBinding (MkBinding a x) y = Let a x y
  (x, _, ctx) <-
    infer' mempty . foldr addBinding (Hole (Unit ())) . bindings $ defs
  let ss = Map.fromList $ signatures defs <&> \(MkSignature a t) -> (a, t)
  fs <- failMaybe $ view localEnv . fst <$> Map.maxView ctx
  th <- failMaybe . unifies $
    Map.intersectionWith (,) (fs <&> \(Poly _ t) -> t) (ss <&> \(Poly _ u) -> u)
  -- TODO: make sure the correct variables are frozen!
  let ctx' = ctx <&>
        over goalType freezeAll
        . over localEnv (freezeUnbound . instantiate th <$>)
        . subst th
  assign contexts ctx'
  y <- etaExpand (strip x)
  -- traceShowM . pretty $ y
  liftEval (eval mempty y) >>= \case
    Scoped m (Hole h) -> do
      assign mainScope m
      modifying contexts $ Map.delete h
      assign examples $ asserts defs
      -- TODO: find reasonable fuel
      cs <- liftUneval 1000 (for (asserts defs) (unevalAssert m)) >>= \case
        Nothing -> fail "Out of fuel"
        Just xs -> mfold . fmap mergeConstraints . dnf $ Conjunction xs
      updateConstraints cs
      -- TODO: how do we deal with running out of fuel? Can we still have
      -- assertions at some nodes of the computation?
      return y
    _ -> error "Should never happen"

-- resHoles :: Result -> Set Hole
-- resHoles = cataExpr \case
--   Scoped m (Hole h) -> Set.singleton h <> scopeHoles m
--   Scoped m _ -> scopeHoles m
--   App f x -> f <> x
--   _ -> mempty

-- scopeHoles :: Scope -> Set Hole
-- scopeHoles = foldMap resHoles

-- constrained :: Constraints -> Set Hole
-- constrained cs = Map.keysSet cs
--   <> let
--     bar = foldMap Map.keys $ toList cs
--     baz = foldMap scopeHoles bar
--   in baz

-- TODO: maybe reintroduce some informativeness constraint
informative :: Set Hole -> Constraints -> Bool
informative hs cs = hs == Map.keysSet cs
-- informative hs cs = hs == constrained cs

updateConstraints :: [Constraints] -> Synth ()
updateConstraints xs = do
  let ys = nubOrd xs
  guard . not . null $ ys
  assign constraints $ Disjunction . fmap Pure $ ys

-- updateConstraints :: [Constraints] -> Synth ()
-- updateConstraints xs = do
--   let ys = nubOrd xs
--   hs <- Map.keysSet <$> use contexts
--   -- Make sure every hole has constraints. ('Informativeness restriction')
--   let zs = filter (informative hs) ys
--   guard . not . null $ zs
--   assign constraints $ Disjunction . fmap Pure $ zs

-- TODO: sometimes it's faster to introduce helper functions rather than
-- eliminators/folds (e.g. introducing `not` in `nat_even`), but other times
-- it is better to just intoduce the fold (e.g. introducing `append` in
-- `tree_collect`)

-- TODO: should we have a weight here? Giving a weight of 1 to
-- constructors and variables makes the benchmarks an order of
-- magnitude slower! The weight of global variables should probably be scaled
-- further compared to constructors and local variables.
refinements :: HoleCtx -> Synth (Term HoleCtx)
refinements (HoleCtx t ctx) = do
  e <- join $ mfold
    -- TODO: make sure that recursive functions do not loop infinitely.
    -- For local variables, introduce a hole for every argument.
    [ do
      (v, Poly _ (Args ts _)) <- mfold $ Map.assocs ctx
      return $ apps (Var v) (Hole (Unit ()) <$ ts)
    -- For concrete types, try the corresponding constructors.
    -- TODO: check if constructors are introduced correctly, see list_map.hs
    -- NOTE: giving constructors a higher weight makes list_sort way faster,
    -- but list_rev_fold seems to diverge.
    , case t of
      Apps (Ctr d) _ -> do
        (_, cs) <- mfold . Map.lookup d =<< view dataTypes
        (c, ts) <- mfold cs
        return $ apps (Ctr c) (Hole (Unit ()) <$ ts)
      _ -> mzero
    -- Global variables are handled like local variables, but only if they are
    -- explicitly imported.
    , do
      fs <- view functions
      ws <- use weights
      (v, (Poly _ (Args ts _), w)) <- mfold $ Map.assocs $
        Map.intersectionWith (,) fs ws
      -- TODO: do we update the weights?
      tell $ fromIntegral $ w + length ts
      return $ apps (Var v) (Hole (Unit ()) <$ ts)
    ]
  -- TODO: in replicate, the second argument has type `a`, but shouldn't unify
  -- with type `Nat`.
  (e', _) <- check ctx e $ Poly [] t
  return $ strip e'

step :: Map Hole (Term Hole) -> Synth ()
step hf = do
  -- Pick one blocking hole and remove it.
  -- traceShowM . pretty =<< use fillings
  s <- use mainScope
  rs <- use examples >>= mapM (liftEval . evalAssert s)
  (m, hole) <- mfold $ foldr (<|>) Nothing (blocking . fst <$> rs)
  ctx <- mfold . Map.lookup hole =<< use contexts
  modifying contexts $ Map.delete hole
  -- Find a refinement to extend the hole filling.
  ref <- refinements ctx
  expr <- etaExpand ref >>= traverseOf holes \c -> (,c) <$> fresh
  modifying contexts (<> Map.fromList (toListOf holes expr))
  let new = Map.singleton hole $ over holes fst expr
  use mainScope
    >>= mapM (liftEval . resume new)
    >>= assign mainScope
  modifying fillings (<> new)
  let hf' = hf <> new
  recHole <- fmap recursive . liftEval . eval m $ over holes fst expr
  case recHole of
    Just _h -> do
      -- traceShowM $ "Recursive position: " <> show _h
      tell 1
      step hf'
    Nothing -> do
      cs <- use constraints
      liftUneval 32 (resumeUneval hf' cs) >>= \case
        Nothing -> do
          -- traceM "Out of fuel"
          tell 1
          step hf'
        Just xs -> do
          let disjunctions = dnf xs
          -- TODO: find a good amount of disjunctions allowed.
          if length disjunctions > 32
            then do
              -- traceM $ "Too many disjunctions: "
              --   <> (fromString . show . length $ disjunctions)
              tell 1
              step hf'
            else updateConstraints $ disjunctions >>= mergeConstraints

-- TODO: add a testsuite testing the equivalence of different kinds and
-- combinations of (un)evaluation resumptions.

final :: SynState -> Bool
final = null . view contexts

synth :: Defs Unit -> Synth (Map Hole (Term Hole))
synth d = init d >> go where
  go :: Synth (Map Hole (Term Hole))
  go = do
    st <- get
    if final st
      then use fillings
      else step mempty >> go
