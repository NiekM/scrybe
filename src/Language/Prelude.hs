module Language.Prelude where

import Import
import Language.Syntax
import Language.Parser
import qualified RIO.Map as Map

-- TODO: select a set of functions to form a dataset/benchmark of functions
-- that we should be able to synthesize, consisting of simple prelude functions
-- such as map, foldr, compose, const, etc. as well as slightly more
-- complicated functions from e.g. Ask-Elle, Haskell99, Myth. Implement model
-- solutions for these functions and generate input output examples to be used
-- for program synthesis.

-- TODO: curate a set of techniques and language constructs used in this set of
-- functions that we would like to be able to learn and would like to be able
-- to extract from model solutions and use to prune the synthesis.

-- TODO: load preludes/modules and such from files.
-- TODO: add pragma's or other restrictions
prelude :: Map Var Poly
prelude = Map.fromList
  $ parseUnsafe ((,) <$> parser <* symbol "::" <*> parser) <$>
  [ "flip :: forall 0 1 2. ({0} -> {1} -> {2}) -> {1} -> {0} -> {2}"
  , "compose :: forall 0 1 2. ({1} -> {2}) -> ({0} -> {1}) -> ({0} -> {2})"
  , "const :: forall 0 1. {0} -> {1} -> {0}"
  , "id :: forall 0. {0} -> {0}"
  , "fix :: forall 0. ({0} -> {0}) -> {0}"
  , "rec :: forall 0 1. (({0} -> {1}) -> {0} -> {1}) -> {0} -> {1}"

  , "true :: forall . Bool"
  , "false :: forall . Bool"

  , "elimBool :: forall 0. {0} -> {0} -> Bool -> {0}"

  , "not :: forall . Bool -> Bool"

  , "zero :: forall . Nat"
  , "succ :: forall . Nat -> Nat"

  , "elimNat :: forall 0. {0} -> (Nat -> {0}) -> Nat -> {0}"

  , "even :: forall . Nat -> Bool"
  , "odd :: forall . Nat -> Bool"

  , "plus :: forall . Nat -> Nat -> Nat"
  , "mult :: forall . Nat -> Nat -> Nat"

  , "nil :: forall 0. List {0}"
  , "cons :: forall 0. {0} -> List {0} -> List {0}"

  , "elimList :: forall 0 1. {0} -> ({1} -> List {1} -> {0}) -> List {1} -> {0}"

  , "foldr :: forall 0 1. ({0} -> {1} -> {1}) -> {1} -> List {0} -> {1}"
  , "map :: forall 0 1. ({0} -> {1}) -> List {0} -> List {1}"
  , "reverse :: forall 0. List {0} -> List {0}"
  , "length :: forall 0. List {0} -> Nat"

  , "pair :: forall 0 1. {0} -> {1} -> Pair {0} {1}"

  , "elimPair :: forall 0 1 2. ({0} -> {2}) -> ({1} -> {2}) -> Pair {0} {1} -> {2}"

  , "fst :: forall 0 1. Pair {0} {1} -> {0}"
  , "snd :: forall 0 1. Pair {0} {1} -> {1}"
  , "swap :: forall 0 1. Pair {0} {1} -> Pair {1} {0}"

  , "curry :: forall 0 1 2. (Pair {0} {1} -> {2}) -> {0} -> {1} -> {2}"
  , "uncurry :: forall 0 1 2. {0} -> {1} -> {2} -> (Pair {0} {1} -> {2})"

  , "zip :: forall 0 1. List {0} -> List {1} -> List (Pair {0} {1})"
  ]
