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

-- | All possible ways to use an expression by applying it to a number of holes
expand :: HasApp e => Expr e (Expr t a) -> Expr t a
  -> [(Expr e (Expr t a), Expr t a)]
expand e t = (e, t) : case t of
  Arr t1 t2 -> expand (App e (Hole t1)) t2
  _ -> []

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

holeFillings :: Ord a => MonadPlus m => Map Var (Type Free) -> Module ->
  m (Term a, Type Free)
holeFillings local Module { ctrs, vars } = mfold . Map.assocs $
  Map.mapKeys Var (vars <> local) <> Map.mapKeys Ctr ctrs

data Syn = Syn
  -- TODO: does init make sense? Maybe we should just have a module as input
  -- and compute the GenState
  { init :: Dec -> GenT Maybe (Term Hole)
  , step :: Term Hole -> GenT [] (Term Hole)
  }

naive :: Syn
naive = Syn
  { init = \dec -> do
    m <- use env
    (expr, _, _, ctx) <- check m dec
    assign holeInfo ctx
    modifying env \Module { ctrs, vars } -> Module
      { ctrs = Map.withoutKeys ctrs . Set.fromList . getCtrs $ expr
      , vars = Map.withoutKeys vars . Set.fromList . getVars $ expr
      }
    return expr

  , step = \ expr -> do
    ctxs <- use holeInfo
    -- Select the first hole
    -- TODO: have some way to better (interactively) choose which goal gets
    -- chosen during synthesis.
    ((i, HoleCtx { goal, local }), ctxs') <- mfold $ Map.minViewWithKey ctxs
    -- TODO: have a better representation of the environment so no duplicate
    -- unification is attempted
    -- Pick an expression from the environment
    (name, t) <- use env >>= holeFillings local
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
    assign holeInfo $ substInfo th <$> (ctxs' <> fmap (`HoleCtx` local) new)
    modifying env $ case name of
      Ctr x -> \m -> m { ctrs = Map.delete x (ctrs m) }
      Var x -> \m -> m { vars = Map.delete x (vars m) }
      _ -> id
    return $ subst (Map.singleton i hf) expr
  }

eta :: Syn
eta = Syn
  { init = \dec -> do
    m <- use env
    (expr, _, _, ctx) <- check m dec
    assign holeInfo ctx
    etaExpand expr

  , step = \expr -> do
    ctxs <- use holeInfo
    -- Select the first hole
    ((i, HoleCtx { goal, local }), _) <- mfold $ Map.minViewWithKey ctxs
    -- Remove selected hole
    modifying holeInfo $ Map.delete i
    -- Pick an expression from the environment
    (name, t) <- use env >>= holeFillings local
    -- Renumber its type to avoid conflicts
    u <- renumber t
    let (args, res) = splitArgs u
    -- Try to unify with the goal type
    th <- unify res goal
    -- Generate new holes
    hs <- for args \arg -> do
      h <- fresh
      modifying holeInfo $ Map.insert h HoleCtx { goal = arg, local }
      return $ Hole h
    hf <- etaExpand (apps $ name :| hs)
    -- hf <- etaExpand _
    modifying holeInfo $ fmap (substInfo th)
    return $ subst (Map.singleton i hf) expr
  }
