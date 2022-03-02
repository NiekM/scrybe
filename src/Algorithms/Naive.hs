module Algorithms.Naive where

import Import
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

selectFirst :: (MonadPlus m, WithHoleCtxs s m) => m (Hole, HoleCtx)
selectFirst = do
  ((i, ctx), _) <- use holeCtxs >>= mfold . Map.minViewWithKey
  modifying holeCtxs $ Map.delete i
  return (i, ctx)

data Syn = Syn
  -- TODO: does init make sense? Maybe we should just have a module as input
  -- and compute the GenState
  { init :: Dec -> GenT Maybe (Term Hole)
  , step :: Term Hole -> GenT [] (Term Hole)
  }

synth :: Syn
synth = Syn
  { init = \dec -> do
    (expr, _, _, ctx) <- check dec
    assign holeCtxs ctx
    processSketch expr
  , step = \expr -> do
    (i, ctx) <- selectFirst
    hf <- pick ctx
    return $ subst (Map.singleton i hf) expr
  }
