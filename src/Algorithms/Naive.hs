module Algorithms.Naive where

import Import hiding (local)
import Language
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import TermGen
import Control.Monad.State

{-

Naive synthesis

This is a naive type-driven synthesizer that tries to fill each hole by
unifying its type with all possible instantiations of expressions in the
environment. It introduces no language constructs (lambda abstractions, case
analyses etc.). It does, however, use every expression in the environment only
once, to emulate simple synthesis parameters.

It is immediately clear how this synthesizer will try to fill in `compose` and
`foldr` at every step of the synthesis, since their return types are
polymorphic.

Rather than using `compose` to synthesize point-free expressions, we should
probably eta-expand each hole, in a sense inlining any compositions. Perhaps
all combinators, such as `id`, `const` and `flip`, should be disallowed, as
they all try to serve the same purpose that lambda abstractions already
provide.

Even though we do not want to use `compose` and friends, `foldr` is a very
useful function that we do want to use during synthesis. It should, however,
come with some restrictions, similar to the ones we would put on case analyses
and recursive calls, since `foldr` uses both internally.

-}

-- | All possible ways to use an expression by applying it to a number of holes
expand :: HasApp e => Expr e (Expr t a) -> Expr t a
  -> [(Expr e (Expr t a), Expr t a)]
expand e t = (e, t) : case t of
  Arr t1 t2 -> expand (App e (Hole t1)) t2
  _ -> []

fromDec :: Dec -> GenT Maybe (Term Hole)
fromDec dec = do
  m <- use env
  (expr, _, _, ctx) <- check m dec
  assign holeInfo ctx
  return expr

getCtrs :: Term Hole -> [Ctr]
getCtrs = \case
  Ctr c -> [c]
  App f x -> getCtrs f ++ getCtrs x
  Lam _ x -> getCtrs x
  _ -> []

getVars :: Term Hole -> [Var]
getVars = \case
  Var v -> [v]
  App f x -> getVars f ++ getVars x
  Lam _ x -> getVars x
  _ -> []

step :: Term Hole -> GenT [] (Term Hole)
step expr = do
  ctxs <- use holeInfo
  -- Select the first hole
  -- TODO: have some way to better (interactively) choose which goal gets
  -- chosen during synthesis.
  ((i, HoleInfo { goal, ctx }), ctxs') <- mfold $ Map.minViewWithKey ctxs
  -- TODO: have a better representation of the environment so no duplicate
  -- unification is attempted
  m <- use env
  let usedVars = Set.fromList . getVars $ expr
  let usedCtrs = Set.fromList . getCtrs $ expr
  let options = Map.mapKeys Var (Map.withoutKeys (vars m) usedVars <> ctx)
             <> Map.mapKeys Ctr (Map.withoutKeys (ctrs m) usedCtrs)
  -- Pick an expression from the environment
  (name, t) <- mfold . Map.assocs $ options
  -- Renumber its type to avoid conflicts
  u <- renumber t
  -- Compute all ways to add holes to the expression
  (ex, typ) <- mfold $ expand name u
  -- Try to unify with the goal type
  th <- unify typ goal
  -- Replace typed holes with numbers
  sk <- number ex
  let hf = fst <$> sk
  let new = Map.fromList . holes $ sk
  assign holeInfo $ substInfo th <$> (ctxs' <> fmap (`HoleInfo` ctx) new)
  return $ subst (Map.singleton i hf) expr

stepEta :: Term Hole -> GenT [] (Term Hole)
stepEta expr = do
  ctxs <- use holeInfo
  -- Select the first hole
  ((i, HoleInfo { goal, ctx }), _) <- mfold $ Map.minViewWithKey ctxs
  -- Remove selected hole
  modifying holeInfo $ Map.delete i
  m <- use env
  let options = Map.mapKeys Var (vars m <> ctx)
             <> Map.mapKeys Ctr (ctrs m)
  -- Pick an expression from the environment
  (name, t) <- mfold . Map.assocs $ options
  -- Renumber its type to avoid conflicts
  u <- renumber t
  let (args, res) = splitArgs u
  -- Try to unify with the goal type
  th <- unify res goal
  -- Generate new holes
  hs <- for args \arg -> do
    h <- fresh @Hole
    modifying holeInfo $ Map.insert h HoleInfo { goal = arg, ctx }
    return $ Hole h
  hf <- etaExpand (apps $ name :| hs)
  modifying holeInfo $ fmap (substInfo th)
  return $ subst (Map.singleton i hf) expr
