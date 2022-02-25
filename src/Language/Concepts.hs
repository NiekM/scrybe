{-# LANGUAGE RankNTypes #-}
module Language.Concepts where

import Import
import Language.Syntax
import Language.Type
import Language.Utils
import qualified RIO.Map as Map
import qualified RIO.Set as Set

-- TODO: keep everything in here abstract

-- NOTE: Just n means n occurences, Nothing means infinite occurences
-- TODO: perhaps rather than have 'infinite' occurences, just remove those
-- concepts from hole fillings
type Occur = Maybe Int
type MultiSet a = Map a Occur

fromSet :: Set a -> MultiSet a
fromSet = Map.fromSet . const . Just $ 1

reduce :: Occur -> Occur -> Maybe Occur
reduce Nothing (Just _) = Just Nothing
reduce (Just n) (Just k) | n > k = Just . Just $ n - k
reduce _ _ = Nothing

sub :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
sub a b = (<> Map.difference a b) . Map.mapMaybe id
  $ Map.intersectionWith reduce a b

-- TODO: how to handle different levels of precision/abstraction? for example,
-- we might have a concept for pattern matching and a more specific concept for
-- pattern matching on a concrete type. similarly for a function/datatype
-- concept vs that same function/datatype but with type parameters
-- instantiated. how do we handle these related concepts in a way that is sound
-- and makes sense?

-- TODO: 'naive' synthesis is actually point-free synthesis using a single
-- occurence for each concept. 'eta' synthesis is actually synthesis that keeps
-- the expression in eta-long form by allowing only eta expansion at function
-- types and disallowing partial application. this can all easily be captured
-- in environments, by seeing eta expansion as a hole filling. 'naive'
-- synthesis simply removes eta-expansions from the environment and 'eta'
-- synthesis removes partial applications (non eta-expansion hole fillings with
-- function types) from the environment. we can make 'eta-long' and
-- 'point-free' functions that add restrictions by transforming the
-- environment.

-- | Concepts within our language, used to categorize hole fillings.
data Concept
  = Function Var
  | Datatype Var
  {-
  | EtaExpansion
  | PartialApplication -- TODO: is a function applied to no arguments considered partially applied?
  -}
  -- Recursion etc.
  -- TODO: should we have hierarchies?
  -- TODO: is HigherArity a concept? Since there is a learning gap between
  -- single arity and higher arity functions.
  deriving (Eq, Ord, Show, Read)

-- Type Indexed {{{

-- TODO: use Term Hole with HoleCtxs instead.
-- TODO: perhaps hole fillings should have a more elaborate type, such as a
-- computation that generates the hole filling in a certain mona, along with
-- restrictions on the generated holes.
-- type HoleFilling = Term (Type Free)
-- newtype HoleFilling = HoleFilling
--   { fill :: forall m s.
--     (FreshHole m, FreshFree m, FreshVar m, WithHoleCtxs s m) =>
--     Map Var (Type Free) -> m (Term Hole)
--   }

-- TODO: leave type indexed lookup fot later
-- -- TODO: take into account that the index should be fresh and normalized
-- newtype TypeIndexed a = TypeIndexed (Map (Type Free) [a])
--   deriving stock (Eq, Ord)
--   deriving stock (Functor, Foldable, Traversable)

-- fromList :: [(Type Free, a)] -> TypeIndexed a
-- fromList = TypeIndexed . Map.fromListWith (++) . fmap (second return)

-- filterTI :: (a -> Bool) -> TypeIndexed a -> TypeIndexed a
-- filterTI f (TypeIndexed m) =
--   TypeIndexed . Map.filter (not . null) . fmap (filter f) $ m

-- lookupTI :: Type Free -> TypeIndexed a -> [(Unify 'Type Free, [a])]
-- lookupTI t (TypeIndexed m) = do
--   k <- Map.keys m
--   th <- unify t k
--   xs <- toList $ Map.lookup k m
--   return (th, xs)

-- }}}

-- data Environment = Environment
--   -- TODO: holefillings should probably have multiple concepts associated with
--   -- them, e.g. the hole filling `foldr ? ?` has the concepts 'foldr',
--   -- 'recursion scheme', 'partial application' and perhaps also 'List' or
--   -- 'Foldable'. We can filter on these concepts based on model solutions,
--   -- student models, or synthesis techniques to prune/guide the synthesis.
--   { fillings :: [(Set Concept, HoleFilling)]
--   -- When searching for some value, remove the corresponding concepts from the
--   -- multiset. If a concept no longer occurs in the concepts, remove all values
--   -- from the type indexed map that correspond to that concept.
--   , cncepts :: MultiSet Concept
--   }

type Environment = [(Var, Type Free, Set Concept)]

-- TODO: the internal representation does not yet matter, but a multiset of
-- concepts is definitely good, and a way to choose holefillings based on these
-- concepts, updating the concepts along the way.

data Technique = PointFree | EtaLong
  deriving (Eq, Ord, Show, Read)

data Options = Options
  { _technique :: Technique
  , _concepts :: MultiSet Concept
  } deriving (Eq, Ord, Show, Read)

class HasOptions a where
  options :: Lens' a Options

class HasConcepts a where
  concepts :: Lens' a (MultiSet Concept)

instance HasConcepts Options where
  concepts = lens _concepts \x y -> x { _concepts = y }

class HasTechnique a where
  technique :: Lens' a Technique

instance HasTechnique Options where
  technique = lens _technique \x y -> x { _technique = y }

type WithOptions s m = (MonadState s m, HasOptions s)

type SynMonad s m = (WithEnvironment s m, WithOptions s m, FillMonad s m)

postProcess :: SynMonad s m => Term Hole -> m (Term Hole)
postProcess e = use (options . technique) >>= \case
  EtaLong -> etaExpand e
  _ -> return e

-- useFunction :: (WithOptions s m, FillMonad s m) =>
--   Var -> Type Free -> HoleCtx -> m (Term Hole)
-- useFunction f t HoleCtx { goal } = do
--   -- Options { available } <- use options
--   let cs = fromSet $ Set.fromList [Function f]
--   modifying (options . concepts) (`sub` cs)
--   -- TODO: restrict environment to updated concepts
--   undefined

type HoleFilling = (Term (Type Free), Type Free)

-- NOTE: this function does not take into account any newly introduced
-- variables in the sketch, this should be done separately with a call to
-- etaExpand
tryHoleFilling :: SynMonad s m => HoleCtx -> HoleFilling -> m (Term Hole)
tryHoleFilling HoleCtx { goal, local } (e, t) = do
  -- TODO: refresh type variables of holefilling?
  th <- unify t goal
  x <- forM e \u -> do
    h <- fresh
    modifying holeCtxs $ Map.insert h HoleCtx { goal = u, local }
    return h
  modifying holeCtxs . fmap $ substCtx th
  return x

-- -- TODO: should this function get a holeCtx as input?
-- withConcepts :: (SynMonad s m, MonadPlus m) => (Var, Type Free, Set Concept) ->
--   m (Set Concept, (Term Hole, Type Free))
-- withConcepts (name, ty, cs) = do
--   xs <- use (options . concepts)
--   msum
--     . fmap (\(c, e) -> (Set.insert (Function name) $ cs <> c,) <$> e)
--     . filter (Set.isSubsetOf (Map.keysSet xs) . fst) $
--     [ ( mempty
--       , _
--       -- let (us, u) = splitArgs ty
--       -- hs <- for us \goal -> do
--       --   h <- fresh
--       --   modifying holeCtxs $ Map.insert h HoleCtx { goal, local }
--       --   return $ Hole h
--       -- (,u) <$> etaExpand (apps $ Var name :| hs)
--       )
--     -- , ( Set.singleton PartialApplication
--     --   , _
--     --   )
--     ]

fullyApply :: (Var, Type Free) -> HoleFilling
fullyApply (name, t) = first (apps . (Var name :|) . fmap Hole) (splitArgs t)

fillHole :: (SynMonad s m, MonadPlus m) => HoleCtx -> m (Term Hole)
fillHole ctx = do
  -- Compute hole fillings from local variables.
  let locals = fmap ((,Set.empty) . fullyApply) . Map.assocs $ local ctx
  -- Compute hole fillings from global variables.
  let globals = use environment >>= \m -> do
      (name, t, c) <- mfold m
      u <- renumber t
      return (fullyApply (name, u), c)
  -- Choose hole fillings from either local or global variables.
  (hf, cs) <- mfold locals <|> globals
  -- Check if the hole fillings fit.
  e <- tryHoleFilling ctx hf
  -- Remove the used concepts.
  modifying (options . concepts) (`sub` fromSet cs)
  -- Remove functions from the environment that use removed concepts
  cs' <- use (options . concepts)
  modifying environment . restrict $ Map.keysSet cs'
  postProcess e

class HasEnvironment a where
  environment :: Lens' a Environment

type WithEnvironment s m = (MonadState s m, HasEnvironment s)
type FillMonad s m =
  (FreshVar m, FreshHole m, FreshFree m, WithHoleCtxs s m, MonadFail m)

restrict :: Set Concept -> Environment -> Environment
restrict cs = filter \(_, _, c) -> c `Set.isSubsetOf` cs

-- pick :: (FillMonad s m, WithEnvironment s m, MonadPlus m) =>
--   HoleCtx -> m (Term Hole)
-- pick ctx = pickLocal ctx <|> do
--   Environment { fillings, cncepts } <- use environment
--   (cs, hf) <- mfold fillings
--   e <- fill hf ctx
--   let concepts' = cncepts `sub` Map.fromSet (const . Just $ 1) cs
--   assign environment Environment
--     { fillings = restrict (Map.keysSet concepts') fillings
--     , cncepts = concepts'
--     }
--   return e

-- pickLocal :: (FillMonad s m, WithEnvironment s m, MonadPlus m) =>
--   HoleCtx -> m (Term Hole)
-- pickLocal ctx = do
--   Environment { cncepts } <- use environment
--   let locals = concatMap (uncurry instantiate . first Var)
--         . Map.assocs $ local ctx
--   (_, hf) <- mfold $ restrict (Map.keysSet cncepts) locals
--   fill hf ctx

-- {{{

-- restrict :: Set Concept -> TypeIndexed (Set Concept, a) ->
--   TypeIndexed (Set Concept, a)
-- restrict cs = filterTI (Set.isSubsetOf cs . fst)

-- remove :: MultiSet Concept -> Environment -> Environment
-- remove cs Environment { typeIndexed, concepts } =
--   let (cs', _) = concepts `sub` cs
--   in Environment
--   { typeIndexed = restrict (Map.keysSet cs') typeIndexed
--   , concepts = cs'
--   }

-- lookup :: Type Free -> Environment ->
--   [(Unify 'Type Free, HoleFilling, Environment)]
-- lookup t e = do
--   (th, xs) <- lookupTI t (typeIndexed e)
--   (cs, hf) <- xs
--   return (th, hf, remove (fromSet cs) e)

-- -- TODO: how do we handle local environments?
-- etaExpand :: (Type Free, HoleFilling)
-- etaExpand = (Arr (Hole 0) (Hole 1),) HoleFilling
--   { fill = \local -> do
--       h <- fresh
--       x <- fresh
--       let ctx = HoleCtx (Hole 0) $ Map.insert x (Hole 1) local
--       modifying holeCtxs $ Map.insert h ctx
--       return $ Lam x (Hole h)
--   }

-- }}}

-- newtype HoleFilling = HoleFilling
--   { fill :: forall s m. FillMonad s m => HoleCtx -> m (Term Hole) }

-- etaEx :: (Set Concept, HoleFilling)
-- etaEx = (Set.singleton EtaExpansion,) $ HoleFilling \case
--   HoleCtx { goal = Arr t u, local } -> do
--     h <- fresh
--     x <- fresh
--     let ctx = HoleCtx t $ Map.insert x u local
--     modifying holeCtxs $ Map.insert h ctx
--     return $ Lam x (Hole h)
--   _ -> fail "Non-function type"

-- TODO: expand does not return 'all' ways to add holes to an
-- expression, since its return type might unify with a function type.
expand :: Term (Type Free) -> Type Free -> [(Term (Type Free), Type Free)]
expand e t = (e, t) : case t of
  Arr t1 t2 -> expand (App e (Hole t1)) t2
  _ -> []

-- instantiate :: Term (Type Free) -> Type Free -> [(Set Concept, HoleFilling)]
-- instantiate x t = go <$> expand x t where
--   go (e, u) = (,fromExpr (free t) e u) $ Set.fromList case u of
--     Arr {} -> [PartialApplication]
--     _      -> []

-- function :: Var -> Type Free -> [(Set Concept, HoleFilling)]
-- function f = fmap (first . Set.insert . Function $ f) . instantiate (Var f)

-- fromExpr :: Set Free -> Term (Type Free) -> Type Free -> HoleFilling
-- fromExpr f e t = HoleFilling \HoleCtx { goal, local } -> do
--   -- Renumber free variables.
--   th0 <- fmap Hole . Map.fromList <$> number (toList f)
--   -- Unify with the goal.
--   th <- unify (subst th0 t) goal
--   -- Extract types from the holes.
--   e' <- number (subst (th `compose` th0) <$> e)
--   let (r, m) = extract e'
--   -- Add new hole contexts.
--   modifying holeCtxs (<> fmap (`HoleCtx` local) m)
--   return r
