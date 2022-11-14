{-# LANGUAGE TemplateHaskell #-}

module Synthesis where

import Import
import Options (SynOptions(..), synPropagate)
import qualified Utils.Weighted as Weighted
import Language
import qualified RIO.Map as Map
import qualified RIO.Set as Set

-- Synthesis Monad {{{

type Fillings = Map Hole (Term Hole)

data SynState = SynState
  { _options     :: SynOptions
  , _contexts    :: Map Hole Goal
  , _constraints :: Logic Constraints
  , _examples    :: [Assert]
  , _included    :: [(Var, Poly)]
  , _forbidden   :: [Map Hole (Term Unit)]
  , _mainScope   :: Scope
  , _fillings    :: Fillings
  -- Fresh variables
  , _freshHole   :: Fresh Hole
  , _freshFree   :: Fresh Free
  , _freshVar    :: Fresh Var
  -- Heuristics
  , _varOnly     :: Set Hole
  , _scrutinized :: Set Var
  , _multiplier  :: Map Hole Dist
  }

makeLenses ''SynState

mkSynState :: SynOptions -> SynState
mkSynState opts = SynState opts mempty (Disjunction []) mempty mempty mempty
  mempty mempty mempty mempty mempty mempty mempty mempty

type Nondet = Weighted.Search Dist
type Synth = RWST Env () SynState Nondet

weigh :: Hole -> Dist -> Synth ()
weigh h d = do
  m <- Map.lookup h <$> use multiplier
  Weighted.weigh $ d * fromMaybe 1 m

runSynth :: SynOptions -> Env -> Synth a -> Nondet a
runSynth opts m x = view _1 <$> runRWST x m (mkSynState opts)

-- }}}

liftInfer :: Infer a -> Synth a
liftInfer = mapRWST mfold . zoom freshFree

liftUneval :: Int -> Uneval a -> Synth (Maybe a)
liftUneval fuel x = ask <&> \e -> view _1 <$> runRWST x e fuel

checkBindings :: [Signature] -> [Binding Unit] -> Infer (Term Goal)
checkBindings ss fs = do
  let sigs = Map.fromList $ ss <&> \(MkSignature a (Poly _ t)) -> (a, t)
  let e = Lets (fs <&> \(MkBinding a x) -> (a, x)) (Hole (Unit ()))
  (_, a) <- infer mempty e
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

init :: Defs Unit -> Synth (Term Hole)
init defs = do
  let ws = [x | Include xs <- pragmas defs, x <- toList xs]

  gs <- asks $ view envFuns
  ws' <- zoom freshFree $ forOf (each . _2 . each) ws refresh

  assign included $ ws' <&> \(v, a) ->
    ( v
    , case Map.lookup v gs of
        Nothing -> error $ "Unknown function " <> show v
        Just b -> case a of
          Nothing -> b
          Just (Poly _ t) | Poly _ u <- b -> case unify t u of
            Nothing ->
              error $ "Included function " <> show v <> " has wrong type"
            Just th -> poly (subst th t)
    )

  -- Type check
  expr <- liftInfer $ checkBindings (signatures defs) (bindings defs)
  -- Eta expand
  expanded <- zoom freshVar $ expand expr
  -- Name holes and extract context
  (e, ctx) <- zoom freshHole $ extract expanded
  assign contexts ctx

  liftEval (eval mempty e) >>= \case
    Scoped m (Hole h) -> do
      assign mainScope m
      modifying contexts $ Map.delete h
      assign examples $ asserts defs
      -- TODO: find reasonable fuel
      cs <- liftUneval 1000 (for (asserts defs) (unevalAssert m)) >>= \case
        Nothing -> fail "Out of fuel"
        Just xs -> mfold . fmap mergeConstraints . dnf $ Conjunction xs
      updateConstraints $ toList cs
      -- TODO: how do we deal with running out of fuel? Can we still have
      -- assertions at some nodes of the computation?
      return e
    _ -> error "Should never happen"

-- TODO: maybe reintroduce some informativeness constraint
informative :: Set Hole -> Constraints -> Bool
informative hs cs = hs == Map.keysSet cs

updateConstraints :: [Constraints] -> Synth ()
updateConstraints xs = do
  let ys = nubOrd xs
  guard . not . null $ ys
  assign constraints $ Disjunction . fmap Pure $ ys

elimWeight, schemeWeight, recVarWeight :: Dist
elimWeight = 4
schemeWeight = 4
recVarWeight = 2

computeWeight :: Type -> Term Goal -> Dist
computeWeight t e = fromIntegral $ sum
  [ length us
  , sum (us <&> \u -> max 0 (typeSize u - typeSize t))
  ] where us = toListOf (holes . goalType) e

refinements :: (Scope, Hole) -> Goal -> Synth (Term Hole)
refinements (m, h) (Goal ctx t) = do
  onlyVar <- Set.member h <$> use varOnly
  (e, ts) <- join $ mfold
    -- Local variables
    [ do
      (v, Poly _ (Args ts _)) <- mfold $ Map.assocs ctx
      when onlyVar do
        guard . Set.notMember v =<< use scrutinized
        modifying scrutinized $ Set.insert v
        modifying varOnly $ Set.delete h
      when (recVar v m) do
        weigh h recVarWeight
      return (Var v, ts)
    -- Constructors
    , case t of
      Apps (Ctr d) _ | not onlyVar -> do
        (_, cs) <- mfold . Map.lookup d =<< view envData
        (c, ts) <- mfold cs
        return (Ctr c, ts)
      _ -> mzero
    -- Imported functions
    , do
      guard $ not onlyVar
      (v, Poly _ (Args ts _)) <- mfold =<< use included
      return (Var v, ts)
    ]
  (_, e1, th) <- liftInfer . check ctx (apps e (Hole <$> ts)) $ Poly [] t
  modifying contexts (subst th <$>)
  e2         <- zoom freshVar  $ expand e1
  (e3, ctx') <- zoom freshHole $ extract e2
  modifying contexts (<> ctx')
  let hs = Map.keys ctx'
  -- Weights
  weigh h $ computeWeight t e1
  mul <- fromMaybe 1 . Map.lookup h <$> use multiplier
  modifying multiplier $ Map.mapWithKey \k d ->
    if k `elem` hs then mul * d else d
  -- Forbidden
  updateForbidden h $ over holes (const $ Unit ()) e1
  case e2 of
    Apps (Var v) _ -> do
      -- Equivalences {{{
      let u = Hole $ Unit ()
      let z = Ctr "Zero"
      let s = App (Ctr "Succ") u
      let n = Ctr "Nil"
      let c = apps (Ctr "Cons") (replicate 2 u)
      case v of
        "plus" | [l, _] <- hs -> do
          let zeros = (`Map.singleton` z) <$> hs
          let succs = (`Map.singleton` s) <$> hs
          let left = Map.singleton l $ apps (Var "plus") (replicate 2 u)
          modifying forbidden (<> (left : zeros ++ succs))
        "mult" | [l, _] <- hs -> do
          let zeros = (`Map.singleton` z) <$> hs
          -- TODO: how to disallow multiplying by 1? We probably need some
          -- more sophisticated representation of forbidden fillings.
          -- let succs = (`Map.singleton` App (Ctr "Succ") u) <$> hs
          let left = Map.singleton l $ apps (Var "mult") (replicate 2 u)
          modifying forbidden (<> (left : zeros))
        "append" | [l, _] <- hs -> do
          let nils = (`Map.singleton` n) <$> hs
          let cons = Map.singleton l c
          let left = Map.singleton l $ apps (Var "append") (replicate 2 u)
          modifying forbidden (<> (left : cons : nils))
        "snoc" | [l, _] <- hs -> do
          let nil = Map.singleton l n
          let cons = Map.singleton l c
          modifying forbidden (<> [nil, cons])
        "even" | [l] <- hs -> do
          let zero = Map.singleton l z
          let succ = Map.singleton l s
          modifying forbidden (<> [zero, succ])
        "length" | [l] <- hs -> do
          let nil = Map.singleton l n
          let cons = Map.singleton l c
          modifying forbidden (<> [nil, cons])
        "sum" | [l] <- hs -> do
          let nil = Map.singleton l n
          let cons = Map.singleton l c
          modifying forbidden (<> [nil, cons])
        "sumrec" | [l] <- hs -> do
          let nil = Map.singleton l n
          let cons = Map.singleton l c
          modifying forbidden (<> [nil, cons])
        "elimTree" | [_, _, l] <- hs -> do
          let leaf = Map.singleton l $ Ctr "Leaf"
          let node = Map.singleton l $ apps (Ctr "Node") (replicate 3 u)
          modifying forbidden (<> [leaf, node])
          modifying varOnly $ Set.insert l
          weigh h elimWeight
        "foldTree" | [_, _, l] <- hs -> do
          let leaf = Map.singleton l $ Ctr "Leaf"
          let node = Map.singleton l $ apps (Ctr "Node") (replicate 3 u)
          modifying forbidden (<> [leaf, node])
          modifying varOnly $ Set.insert l
          weigh h schemeWeight
        "mapTree" | [_, l] <- hs -> do
          let leaf = Map.singleton l $ Ctr "Leaf"
          let node = Map.singleton l $ apps (Ctr "Node") (replicate 3 u)
          let fuse = Map.singleton l $ apps (Var "mapTree") (replicate 2 u)
          modifying forbidden (<> [leaf, node, fuse])
          modifying varOnly $ Set.insert l
        "elimList" | [_, _, l] <- hs -> do
          let nil = Map.singleton l n
          let cons = Map.singleton l c
          modifying forbidden (<> [nil, cons])
          modifying multiplier $ Map.insert l 2
          weigh h elimWeight
        "foldList" | _:_:l:_ <- hs -> do
          let nil = Map.singleton l n
          let cons = Map.singleton l c
          modifying forbidden (<> [nil, cons])
          modifying varOnly $ Set.insert l
          weigh h schemeWeight
        "foldr" | _:_:l:_ <- hs -> do
          let nil = Map.singleton l n
          let cons = Map.singleton l c
          modifying forbidden (<> [nil, cons])
          modifying varOnly $ Set.insert l
          weigh h schemeWeight
        "foldl" | _:_:l:_ <- hs -> do
          let nil = Map.singleton l n
          let cons = Map.singleton l c
          modifying forbidden (<> [nil, cons])
          modifying varOnly $ Set.insert l
          weigh h schemeWeight
        "paraList" | [_, _, l] <- hs -> do
          let nil = Map.singleton l n
          let cons = Map.singleton l c
          modifying forbidden (<> [nil, cons])
          modifying varOnly $ Set.insert l
          weigh h schemeWeight
        "map" | [_, l] <- hs -> do
          let nil = Map.singleton l n
          let cons = Map.singleton l c
          let fuse = Map.singleton l $ apps (Var "map") (replicate 2 u)
          modifying forbidden (<> [nil, cons, fuse])
        "concat" | [l] <- hs -> do
          let nil = Map.singleton l n
          modifying forbidden (<> [nil])
        "concatMap" | [f, l] <- hs -> do
          let empty = Map.singleton f n
          let nil = Map.singleton l n
          let cons = Map.singleton l c
          modifying forbidden (<> [empty, nil, cons])
        "zipWith" | [_, l, r] <- hs -> do
          let left = Map.singleton l c
          let right = Map.singleton r c
          let fuse = Map.singleton l $ apps (Var "zipWith") (replicate 3 u)
          modifying forbidden (<> [left, right, fuse])
        "elimNat" | [_, _, l] <- hs -> do
          let zero = Map.singleton l z
          let succ = Map.singleton l s
          modifying forbidden (<> [zero, succ])
          modifying multiplier $ Map.insert l 2
          weigh h elimWeight
        "foldrNat" | _:_:l:_ <- hs -> do
          let zero = Map.singleton l z
          let succ = Map.singleton l s
          modifying forbidden (<> [zero, succ])
          modifying varOnly $ Set.insert l
          weigh h schemeWeight
        "elimBool" | [_, _, l] <- hs -> do
          let false = Map.singleton l $ Ctr "False"
          let true = Map.singleton l $ Ctr "True"
          modifying forbidden (<> [false, true])
          modifying multiplier $ Map.insert l 2
          weigh h elimWeight
        "elimOrd" | [_, _, _, l] <- hs -> do
          let lt = Map.singleton l $ Ctr "LT"
          let eq = Map.singleton l $ Ctr "EQ"
          let gt = Map.singleton l $ Ctr "GT"
          modifying forbidden (<> [lt, eq, gt])
          modifying multiplier $ Map.insert l 2
          weigh h elimWeight
        "compareNat" | [x, y] <- hs -> do
          let zz = Map.fromList [(x, z), (y, z)]
          let zs = Map.fromList [(x, z), (y, s)]
          let sz = Map.fromList [(x, s), (y, z)]
          let ss = Map.fromList [(x, s), (y, s)]
          -- NOTE: since eq is commutative, we can disallow one Succ/Zero,
          -- since two is already forbidden
          let zero = Map.singleton x z
          let succ = Map.singleton x s
          modifying forbidden (<> [zero, succ, zz, zs, sz, ss])
        "eq" | [x, y] <- hs -> do
          let zz = Map.fromList [(x, z), (y, z)]
          let zs = Map.fromList [(x, z), (y, s)]
          let sz = Map.fromList [(x, s), (y, z)]
          let ss = Map.fromList [(x, s), (y, s)]
          -- NOTE: since eq is commutative, we can disallow one Succ/Zero,
          -- since two is already forbidden
          let zero = Map.singleton x z
          let succ = Map.singleton x s
          modifying forbidden (<> [zero, succ, zz, zs, sz, ss])
        "neq" | [x, y] <- hs -> do
          let zz = Map.fromList [(x, z), (y, z)]
          let zs = Map.fromList [(x, z), (y, s)]
          let sz = Map.fromList [(x, s), (y, z)]
          let ss = Map.fromList [(x, s), (y, s)]
          -- NOTE: since neq is commutative, we can disallow one Succ/Zero,
          -- since two is already forbidden
          let zero = Map.singleton x z
          let succ = Map.singleton x s
          modifying forbidden (<> [zero, succ, zz, zs, sz, ss])
        "max" | [x, y] <- hs -> do
          let zeroes = (`Map.singleton` z) <$> hs
          -- NOTE: since max is commutative, we can disallow one Succ, since
          -- two is already forbidden
          let succ = Map.singleton x s
          let ss = Map.fromList [(x, s), (y, s)]
          let left = Map.singleton x $ apps (Var "max") (replicate 2 u)
          modifying forbidden (<> (left:succ:ss:zeroes))
        "leq" | [x, y] <- hs -> do
          let zero = Map.singleton x z
          let succs = Map.fromList [(x, s), (y, s)]
          modifying forbidden (<> [zero, succs])
        "any" | [f, l] <- hs -> do
          let false = Map.singleton f $ Ctr "False"
          let true = Map.singleton f $ Ctr "True"
          let nil = Map.singleton l n
          modifying forbidden (<> [false, true, nil])
        "or" | [_, _] <- hs -> do
          let falses = (`Map.singleton` Ctr "False") <$> hs
          let trues = (`Map.singleton` Ctr "True") <$> hs
          modifying forbidden (<> falses <> trues)
        "elem" | [_, l] <- hs -> do
          let nil = Map.singleton l n
          let cons = Map.singleton l c
          modifying forbidden (<> [nil, cons])
        "elimMoney" | [_, _, _, l] <- hs -> do
          let one = Map.singleton l $ Ctr "One"
          let two = Map.singleton l $ Ctr "Two"
          let chk = Map.singleton l $ App (Ctr "Check") u
          modifying forbidden (<> [one, two, chk])
          weigh h elimWeight
        _ -> return ()
      -- }}}
    _ -> return ()
  --
  return e3

removeMatching :: (Ord k, Eq v) => k -> v -> Map k v -> Maybe (Map k v)
removeMatching k v m = case Map.lookup k m of
  Nothing -> return m
  Just x -> do
    guard $ x == v
    return $ Map.delete k m

updateForbidden :: Hole -> Term Unit -> Synth ()
updateForbidden h e = do
  fs <- use forbidden
  let fs' = mapMaybe (removeMatching h e) fs
  guard $ not . any null $ fs'
  assign forbidden fs'

unevalFuel :: Int
unevalFuel = 32

-- TODO: find a good amount of disjunctions allowed.
maxDisjunctions :: Int
maxDisjunctions = 32

step :: Fillings -> Synth ()
step hf = do
  -- Pick one blocking hole and remove it.
  s <- use mainScope
  rs <- use examples >>= mapM (liftEval . evalAssert s)
  -- TODO: if there are multiple blocking holes, we could compute hole
  -- dependencies and base the order on that
  (m, hole) <- mfold $ foldr (<|>) Nothing (blocking . fst <$> rs)
  ctx <- mfold . Map.lookup hole =<< use contexts
  modifying contexts $ Map.delete hole
  -- Find a possible refinement.
  expr <- refinements (m, hole) ctx
  -- Update hole fillings.
  let new = Map.singleton hole expr
  use mainScope
    >>= mapM (liftEval . resume new)
    >>= assign mainScope
  modifying fillings (<> new)
  let hf' = hf <> new
  -- NOTE: we eagerly fill in scrutinized holes. Does that make sense? In some
  -- cases it might be better to unevaluate, so that we know better how to fill
  -- the scrutinized hole... TODO: find some good examples
  scrHole <- scrutinizedHole <$> liftEval do
    eval m expr >>= resume new

  cs <- use constraints
  ctxs <- use contexts
  propagate <- use $ options . synPropagate

  case scrHole of
    Just _h -> step hf'
    Nothing
      | not propagate && not (Map.null ctxs) -> step hf'
      | otherwise -> do
        liftUneval unevalFuel (resumeUneval hf' cs) >>= \case
          Nothing -> do
            weigh hole 1
            step hf'
          Just xs
            | length disjunctions > maxDisjunctions -> do
              -- NOTE: weights are added a bit too late. we add a weight
              -- for e.g. using a recursively defined variable, but that's
              -- because unevaluation takes so long, which will happen
              -- anyways, because the weight only affects hole fillings
              -- after that!
              weigh hole 4
              step hf'
            | otherwise ->
              updateConstraints $ disjunctions >>= toList . mergeConstraints
            where disjunctions = dnf xs

final :: SynState -> Bool
final = null . view contexts

synth :: Defs Unit -> Synth Fillings
synth d = init d >> go where
  go :: Synth Fillings
  go = do
    st <- get
    if final st
      then use fillings
      else step mempty >> go

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

-- TODO: add a testsuite testing the equivalence of different kinds and
-- combinations of (un)evaluation resumptions.
