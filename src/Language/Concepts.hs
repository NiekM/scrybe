{-# LANGUAGE RankNTypes #-}
module Language.Concepts where

import Import
import Language.Syntax
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import Prettyprinter

-- TODO: keep everything in here abstract

-- NOTE: Just n means n occurences, Nothing means infinite occurences
-- TODO: perhaps rather than have 'infinite' occurences, just remove those
-- concepts from hole fillings
type Occur = Maybe Int
type MultiSet a = Map a Occur

fromSet :: Set a -> MultiSet a
fromSet = Map.fromSet . const . Just $ 1

fromList :: Ord a => [a] -> MultiSet a
fromList = fmap Just . Map.fromListWith (+) . fmap (,1)

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

-- TODO: should the concepts of a function contain all the concepts in its
-- body, or should we have an abstraction boundary, since e.g. foldr uses
-- direct structural recursion internally, but should only have a recursion
-- scheme concept.

-- | Concepts within our language, used to categorize hole fillings.
data Concept
  = Func Var
  -- | Combinators that only use lambda abstractions and applications.
  | Combinator
  -- Recursion etc.
  -- TODO: should we have hierarchies?
  -- TODO: is HigherArity a concept? Since there is a learning gap between
  -- single arity and higher arity functions.
  deriving (Eq, Ord, Show, Read)

instance Pretty Concept where
  pretty = viaShow

-- TODO: should constructors be allowed? e.g. curry and uncurry are also
-- combinators in some sense.
-- | Check if an expression is a combinator using at most some set of
-- variables.
comb :: Term Void -> Set Var -> Bool
comb = cataExpr \b as -> case b of
  Var a -> a `elem` as
  App f x -> f as && x as
  Lam a x -> x (Set.insert a as)
  _ -> False

type Env = Map Var (Poly, Set Concept)

fromModule :: Mod -> Env
fromModule m = flip Map.mapWithKey (funs_ m)
  \x t -> (t,) . Set.fromList . catMaybes $ [Just (Func x)]

restrict :: Set Concept -> Env -> Env
restrict cs = Map.filter \(_, c) -> c `Set.isSubsetOf` cs

-- TODO: gather the concepts from the prelude, e.g. recognizing that id, const,
-- flip and compose are combinators, so we should not allow them during
-- synthesis, even if they are used in the model solution.

-- TODO: actually gather concepts from variables/constructors/language
-- constructs, by looking up the corresponding concepts in the prelude and
-- filtering out prohibited concepts such as combinators.
fromSketch :: Mod -> Ann Type ('Term a) -> (Env, MultiSet Concept)
fromSketch m e =
  ( Map.fromList xs
  , fromList . concatMap (toList . snd . snd) . toList $ xs
  ) where
  fs = fromModule m
  xs = collect e >>= \(Annot x u) -> case x of
    Var v | Just (_, cs) <- Map.lookup v fs -> [(v, (Poly [] u, cs))]
    _ -> []

class HasConcepts a where
  concepts :: Lens' a (MultiSet Concept)

class HasEnv a where
  environment :: Lens' a Env
