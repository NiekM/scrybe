module Algorithms.Naive where

import Import hiding (local)
import Language
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import TermGen

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

fromSketch :: Dec -> GenT Maybe Sketch
fromSketch dec = do
  (expr, _, _, ctx) <- check dec
  return Sketch { expr, ctx }

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

step :: Sketch -> GenT [] Sketch
step Sketch
  { expr
  , ctx
  }
  = do
    -- Select the first hole
    -- TODO: have some way to better (interactively) choose which goal gets
    -- chosen during synthesis.
    ((i, (goal, local)), ctx') <- mfold $ Map.minViewWithKey ctx
    -- TODO: have a better representation of the environment so no duplicate
    -- unification is attempted
    env <- ask
    let usedVars = Set.fromList . getVars $ expr
    let usedCtrs = Set.fromList . getCtrs $ expr
    let options = Map.mapKeys Var (Map.withoutKeys (vars env) usedVars <> local)
               <> Map.mapKeys Ctr (Map.withoutKeys (ctrs env) usedCtrs)
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
    return Sketch
      { expr = subst (Map.singleton i hf) expr
      , ctx = (subst th *** fmap (subst th)) <$> (ctx' <> fmap (,local) new)
      } -- TODO: have some nicer way to do substitution for holectxs
