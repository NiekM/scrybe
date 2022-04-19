module Language.Live where

import Import hiding (reverse)
import Language.Syntax
import RIO.List
import qualified RIO.Map as Map

{-# COMPLETE Scoped, App, Ctr, Fix #-}
pattern Scoped :: Map Var Result -> x -> Expr' r 'Det v (Annot x Scope)
pattern Scoped m e = Hole (Annot e (Scope m))

pattern Top :: Example
pattern Top = Hole (Unit ())

evalApp :: Result -> Result -> Result
evalApp f x = case f of
  App Fix (Scoped m (Lam g (Lam y e))) ->
    eval (Map.fromList [(g, f), (y, x)] <> m) e
  Scoped m (Lam a y) -> eval (Map.insert a x m) y
  Scoped m (Elim xs)
    | Apps (Ctr c) as <- x
    , Just (Lams bs y) <- lookup c xs
    -> eval (Map.fromList (zip bs as) <> m) y
  r -> App r x

eval :: Map Var Result -> Term Var Hole -> Result
eval m = \case
  Var v -> fromMaybe undefined $ Map.lookup v m
  App f x -> evalApp (eval m f) (eval m x)
  Ctr c -> Ctr c
  Fix -> Fix
  Let _a _x _y -> undefined
  -- Indeterminate results
  Hole h  -> Scoped m $ Hole h
  Lam a x -> Scoped m $ Lam a x
  Elim xs -> Scoped m $ Elim xs

-- TODO: Note that resumption loops forever if the hole fillings are (mutually)
-- recursive. An alternative would be to only allow resumption of one hole at a
-- time.
resume :: Map Hole (Term Var Hole) -> Result -> Result
resume hf = cataExpr \case
  App f x -> evalApp f x
  Ctr c -> Ctr c
  Fix -> Fix
  InvCtr c -> InvCtr c
  -- Indeterminate results
  Scoped m (Hole h)
    | Just x <- Map.lookup h hf
    , x /= Hole h -> resume hf (eval m x)
  Scoped m e -> Scoped (resume hf <$> m) e

upcast :: Value -> Result
upcast = cataExpr \case
  Hole h -> absurd h
  Ctr c -> Ctr c
  App f x -> App f x

downcast :: Result -> Maybe Value
downcast = cataExprM \case
  Ctr c -> return $ Ctr c
  App f x -> return $ App f x
  _ -> Nothing

-- | Hole fillings
type HF = Map Hole (Term Var Hole)

-- | Unfilled holes
type UH = Map Hole Constraint

satExample :: HF -> Result -> Example -> Bool
satExample hf r ex = case (r, ex) of
  (_, Top) -> True
  (Apps (Ctr c) xs, Apps (Ctr d) ys) ->
    c == d && and (zipWith (satExample hf) xs ys)
  (_, Lam v x) -> satExample hf (resume hf (App r (upcast v))) x
  _ -> False

type Constraint = [(Map Var Result, Example)]

-- | Unevaluation constraints
type UC = (UH, HF)

-- TODO: are the holefillings here needed?
satConstraint :: HF -> Term Var Hole -> Constraint -> Bool
satConstraint hf e = all \(m, ex) -> satExample hf (resume hf $ eval m e) ex

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

checkLive :: Term Var Hole -> Constraint -> [UC]
checkLive e = mapMaybe merge . mapM \(env, ex) -> uneval (eval env e) ex

-- TODO: maybe replace Lam by Fix and have Lam be a pattern synonym that sets
-- the recursive argument to Nothing. This makes many things more messy
-- unfortunately.

uneval :: Result -> Example -> [UC]
uneval = curry \case
  -- Top always succeeds.
  (_, Top) -> [mempty]
  -- Constructors are handled as opaque functions and their unevaluation is
  -- extensional in the sense that only their arguments are compared.
  (Apps (Ctr c) xs, Lams ys (Apps (Ctr d) zs))
    | c == d, length xs + length ys == length zs
    -> mapMaybe merge $ zipWithM uneval (xs ++ map upcast ys) zs
  -- Holes are simply added to the environment, with their arguments as inputs.
  -- The arguments should be values in order to obtain correct hole
  -- constraints.
  (Apps (Scoped m (Hole h)) (mapM downcast -> Just vs), ex) ->
    [(Map.singleton h [(m, lams vs ex)], mempty)]
  -- Functions should have both input and output, and evaluating their body on
  -- this input should unevaluate onto the output example.
  (Scoped m (Lam a x), Lam v y) -> checkLive x [(Map.insert a (upcast v) m, y)]
  -- Fixed points additionally add their own definition to the environment.
  (App Fix r@(Scoped m (Lam f (Lam a x))), Lam v y) ->
    checkLive x [(Map.fromList [(f, r), (a, upcast v)] <> m, y)]
  -- (Scoped m (Elim xs), Lam v y) -> undefined -- TODO:
  _ -> []
