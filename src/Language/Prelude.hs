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
prelude :: [Def]
prelude = parseUnsafe parser <$>
  [ "flip :: forall 0 1 2. ({0} -> {1} -> {2}) -> {1} -> {0} -> {2} = \\f. \\x. \\y. f y x"
  , "compose :: forall 0 1 2. ({1} -> {2}) -> ({0} -> {1}) -> ({0} -> {2}) = \\f. \\g. \\x. f (g x)"
  , "const :: forall 0 1. {0} -> {1} -> {0} = \\x. \\y. x"
  , "id :: forall 0. {0} -> {0} = \\x. x"
  , "fix :: forall 0. ({0} -> {0}) -> {0} = fix"
  , "rec :: forall 0 1. (({0} -> {1}) -> {0} -> {1}) -> {0} -> {1} = fix"

  , "true :: forall . Bool = True"
  , "false :: forall . Bool = False"

  , "elimBool :: forall 0. {0} -> {0} -> Bool -> {0} = \\f. \\t. \\b. [b] False => f; True => t"

  , "not :: forall . Bool -> Bool = elimBool True False"

  , "zero :: forall . Nat = Zero"
  , "succ :: forall . Nat -> Nat = Succ"

  , "elimNat :: forall 0. {0} -> (Nat -> {0}) -> Nat -> {0} = elimNat"
  , "foldNat :: forall 0. {0} -> ({0} -> {0}) -> Nat -> {0} = foldNat"

  , "even :: forall . Nat -> Bool = even"
  , "odd :: forall . Nat -> Bool = odd"

  , "plus :: forall . Nat -> Nat -> Nat = plus"
  , "mult :: forall . Nat -> Nat -> Nat = mult"

  , "nil :: forall 0. List {0} = Nil"
  , "cons :: forall 0. {0} -> List {0} -> List {0} = Cons"

  , "elimList :: forall 0 1. {0} -> ({1} -> List {1} -> {0}) -> List {1} -> {0} = elimList"

  , "foldr :: forall 0 1. ({0} -> {1} -> {1}) -> {1} -> List {0} -> {1} = foldr"
  , "map :: forall 0 1. ({0} -> {1}) -> List {0} -> List {1} = \\f. foldr (\\x. Cons (f x)) Nil"
  , "reverse :: forall 0. List {0} -> List {0} = reverse"
  , "length :: forall 0. List {0} -> Nat = foldr (\\x. \\r. Succ r) Zero"

  , "pair :: forall 0 1. {0} -> {1} -> Pair {0} {1} = Pair"

  , "fst :: forall 0 1. Pair {0} {1} -> {0} = \\p. [p] Pair {x} {y} => x"
  , "snd :: forall 0 1. Pair {0} {1} -> {1} = \\p. [p] Pair {x} {y} => y"
  , "swap :: forall 0 1. Pair {0} {1} -> Pair {1} {0} = \\p. [p] Pair {x} {y} => Pair y x"

  , "curry :: forall 0 1 2. (Pair {0} {1} -> {2}) -> {0} -> {1} -> {2} = \\f. \\x. \\y. f (Pair x y)"
  , "uncurry :: forall 0 1 2. ({0} -> {1} -> {2}) -> Pair {0} {1} -> {2} = \\f. \\p. [p] Pair {x} {y} => f x y"

  , "zip :: forall 0 1. List {0} -> List {1} -> List (Pair {0} {1}) = zip"
  ]
