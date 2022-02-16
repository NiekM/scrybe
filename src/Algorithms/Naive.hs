module Algorithms.Naive where

import Import hiding (local)
import Language
import RIO.List
import qualified RIO.Map as Map
import Control.Monad.State
import TermGen

-- TODO: add environment in here, or at least keep a specific test suit for
-- each kind of synthesizer.

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

data GenSt = GenSt
  { expr :: Term Hole
  , goals :: Map Hole (Type Hole)
  , datatypes :: Map Ctr (Type Hole)
  , global :: Map Var (Type Hole)
  , locals :: Map Hole (Map Var (Type Hole))
  , maxHole :: Hole
  , maxFree :: Hole
  } deriving (Eq, Ord, Show)

-- | All possible ways to use an expression by applying it to a number of holes
expand :: HasApp e => Expr e (Expr t a) -> Expr t a
  -> [(Expr e (Expr t a), Expr t a)]
expand e t = (e, t) : case t of
  Arr t1 t2 -> expand (App e (Hole t1)) t2
  _ -> []

instance Gen GenSt where
  fromSketch :: Module -> Dec -> GenSt
  fromSketch m@Module { ctrs, vars } Dec { def, sig } = GenSt
    { expr = def
    , goals = fst <$> ctx
    , datatypes = ctrs
    , global = vars
    , locals = snd <$> ctx
    , maxHole
    , maxFree = 0 -- TODO: compute from type checking result
    } where
      ((_, _, ctx), maxHole) =
        fromMaybe undefined $ runStateT (check m def sig) 0

  result :: GenSt -> Term Hole
  result = expr

  step :: GenSt -> [GenSt]
  step GenSt
    { expr
    , goals
    , datatypes
    , global
    , locals
    , maxHole
    , maxFree
    } = do
      -- Select the first goal
      ((n, goal), goals') <- toList $ Map.minViewWithKey goals
      -- Select the corresponding environment
      local <- toList $ locals Map.!? n
      let env = Map.mapKeys Var (global <> local) <> Map.mapKeys Ctr datatypes
      -- Pick an entry from the environment
      (name, t) <- Map.toList env
      -- Renumber the type variables in ty
      let t' = (+ maxFree) <$> t
      -- Generate all ways to instantiate sketch
      (sketch, typ) <- expand name t'
      -- Check that the type unifies with the goal
      th <- toList $ unify typ goal
      -- Compute new maximum Free variable
      let maxFree' = fromMaybe 0 . maximumMaybe . free $ typ
      let sk = evalState (number sketch) maxHole
      let hf = fst <$> sk
      let new = Map.fromList . holes $ sk
      -- Copy the context to new holes
      let locals' = local <$ new
      return GenSt
        { expr = subst (Map.singleton n hf) expr
        , goals = subst th <$> (goals' <> new)
        , datatypes = case name of Ctr x -> Map.delete x datatypes; _ -> datatypes
        -- Delete the used expression from the environment.
        , global = case name of Var x -> Map.delete x global; _ -> global
        -- Remove the filled hole's context
        , locals = Map.delete n locals <> locals'
        , maxHole = maxHole + fromIntegral (length new)
        , maxFree = maxFree + maxFree'
        }
