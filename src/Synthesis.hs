{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}

module Synthesis where

import Import
import Language
import Nondet
import qualified RIO.Map as Map
import Control.Monad.Heap
import Control.Monad.State
import Data.Monus.Dist

-- TODO: maybe we also want to keep track of a list of possible refinements
-- e.g. [Term Unit]
data SynState = SynState
  { _contexts    :: Map Hole HoleCtx
  , _constraints :: [Constraints]
  , _fillings    :: Map Hole (Term Hole)
  , _freshSt     :: FreshState
  , _mainScope   :: Map Var Result
  , _examples    :: [Assert]
  , _weights     :: Map Var Int
  }

makeLenses ''SynState

emptySynState :: SynState
emptySynState = SynState mempty mempty mempty mkFreshState mempty [] mempty

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

liftUneval :: Int -> Uneval a -> Synth (Nondet a)
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
  let addBinding (MkBinding a x) = Let a x
  (x, _, ctx) <-
    infer' mempty . foldr addBinding (Hole (Unit ())) . bindings $ defs
  ts <- mapM refresh . Map.fromList $ signatures defs <&>
    \(MkSignature a t) -> (a, t)
  th <- failMaybe . unifies $ collect x & mapMaybe \a -> do
    Annot (Lam v _) (Arr t _) <- Just a
    Poly _ u <- Map.lookup v ts
    return (u, t)
  assign contexts $ substCtx th <$> ctx
  y <- etaExpand (strip x)
  liftEval (eval mempty y) >>= \case
    Scoped m (Hole h) -> do
      assign mainScope m
      modifying contexts $ Map.delete h
      assign examples $ asserts defs
      -- NOTE: we explicitly run the reader transformer, so that we can
      -- instantiate the MonadPlus constraint to list, avoiding the mixing of
      -- two different types of nondeterminism, namely the nondeterministic
      -- unevaluation constraints and the nondeterminism of the synthesis
      -- procedure.
      -- TODO: find reasonable fuel
      as <- liftUneval 100 $ -- TODO: find reasonable fuel
        mergeConstraints <$> for (asserts defs) (unevalAssert m)
      -- TODO: how do we deal with running out of fuel? Can we still have
      -- assertions at some nodes of the computation?
      updateConstraints $ catMaybes (either error id . runNondet $ as)
      return y
    _ -> error "Should never happen"

updateConstraints :: [Constraints] -> Synth ()
updateConstraints xs = do
  cs <- Map.keysSet <$> use contexts
  -- Make sure every hole has constraints. ('Informativeness restriction')
  let ys = filter ((== cs) . Map.keysSet) xs
  guard . not . null $ ys
  assign constraints ys

computeBlocking :: Map Var Result -> Assert -> Eval (Maybe Hole)
computeBlocking rs (MkAssert e (Lams vs _)) = do
  v <- eval rs e
  r <- resume mempty $ apps v (upcast <$> vs)
  return $ blocking r

-- TODO: should we have a weight here? Giving a weight of 1 to
-- constructors and variables makes the benchmarks an order of
-- magnitude slower! The weight of global variables should probably be scaled
-- further compared to constructors and local variables.
refinements :: HoleCtx -> Synth (Term HoleCtx)
refinements (HoleCtx t ctx) = do
  e <- join $ mfold
    -- For concrete types, try the corresponding constructors.
    -- TODO: check if constructors are introduced correctly, see list_map.hs
    [ case t of
      Apps (Ctr d) _ -> do
        (_, cs) <- mfold . Map.lookup d =<< view dataTypes
        (c, ts) <- mfold cs
        return $ apps (Ctr c) (Hole (Unit ()) <$ ts)
      _ -> mzero
    -- TODO: make sure that recursive functions do not loop infinitely.
    -- For local variables, introduce a hole for every argument.
    , do
      (v, Args ts _) <- mfold $ Map.assocs ctx
      return $ apps (Var v) (Hole (Unit ()) <$ ts)
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
  s <- use mainScope
  hs <- use examples >>= mapM (liftEval . computeBlocking s)
  hole <- mfold $ foldr (<|>) Nothing hs
  ctx <- mfold . Map.lookup hole =<< use contexts
  modifying contexts $ Map.delete hole
  -- Find a refinement to extend the hole filling.
  ref <- refinements ctx
  expr <- etaExpand ref >>= traverseOf holes \c -> (,c) <$> fresh
  modifying contexts (<> Map.fromList (toListOf holes expr))
  let hf' = Map.insert hole (over holes fst expr) hf
  use mainScope
    >>= traverse (liftEval . resume hf')
    >>= assign mainScope
  modifying fillings (<> hf')
  cs <- use constraints
  -- TODO: find a good amount of fuel and amount of ND allowed.
  liftUneval 8 (mfold cs >>= resumeUneval hf') >>= \case
    Nondet (Right cs')
      -- If there is too much non-determinism, fill another hole before
      -- unevaluating.
      | length cs' > 16 -> traceM "Too much ND!" >> step hf'
      | otherwise -> updateConstraints cs'
    -- When running out of fuel, fill another hole before unevaluating.
    Nondet (Left _) -> traceM "Out of fuel" >> step hf'

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
