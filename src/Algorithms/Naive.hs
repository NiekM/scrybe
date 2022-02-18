module Algorithms.Naive where

import Import hiding (local)
import Language
import qualified RIO.Map as Map
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

fromSketch :: Module -> Dec -> GenT Maybe Sketch
fromSketch env dec  = do
  (expr, _, _, ctx) <- check env dec
  return Sketch { expr, env, ctx }

step :: Sketch -> GenT [] Sketch
step Sketch
  { expr
  , env
  , ctx
  }
  = do
    -- Select the first hole
    ((i, (goal, local)), ctx') <- mfold $ Map.minViewWithKey ctx
    let options = Map.mapKeys Var (vars env <> local)
               <> Map.mapKeys Ctr (ctrs env)
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
      , env = env
        -- Remove used expressions from the environment
        { ctrs = (case name of Ctr x -> Map.delete x; _ -> id) $ ctrs env
        , vars = (case name of Var x -> Map.delete x; _ -> id) $ vars env
        }
      , ctx = (subst th *** fmap (subst th)) <$> (ctx' <> fmap (,local) new)
      }
