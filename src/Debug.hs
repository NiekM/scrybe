{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- NOTE: this module is purely for debugging purposes and to load some
-- functions and instances into ghci
module Debug where

import Import
import Language hiding (free)
import qualified RIO.Text as T
import System.IO.Unsafe
import qualified RIO.Map as Map
import Synthesis
import Utils.Weighted
import Control.Monad.Heap
import qualified Prettyprinter as Pretty

fromStr :: Parse a => String -> a
fromStr = fromMaybe (error "Parse failed") . lexParse parser . T.pack

newtype Parseable a = Parseable a

instance Parse a => IsString (Parseable a) where
  fromString = Parseable . fromStr

deriving via Parseable (Expr l) instance Parse (Expr l) => IsString (Expr l)
deriving via Parseable Poly     instance IsString Poly
deriving via Parseable Assert   instance IsString Assert

prelude :: Env
prelude = let file = unsafePerformIO $ readFileUtf8 "data/prelude.hs" in
  case lexParse (fromDefs . recDefs <$> parser) file of
    Just x -> x
    Nothing -> error "Could not parse prelude"

tryUneval :: Uneval a -> Maybe a
tryUneval x = view _1 <$> runRWST x prelude 1000

uneval' :: Result -> Example -> Maybe (Logic Constraints)
uneval' r e = tryUneval (uneval r $ toEx e)

assert' :: Assert -> Maybe (Logic Constraints)
assert' = tryUneval . unevalAssert mempty

gen' :: String -> (Scope, Space (Term Hole))
gen' f = let file = unsafePerformIO $ readFileUtf8 f in
  case lexParse parser file of
    Just x -> case runRWST (init_ x) prelude mkGenSt of
      Just (e, st, _) -> case runEval prelude (eval mempty e) of
        Scoped s _ -> (s, runReader (gen prelude) st)
        _ -> error "Something went really wrong in evaluation"
      Nothing -> error "Initialization failed"
    Nothing -> error "Could not parse problem"

cegisLoop :: [Assert] -> Space Scope -> [Pretty.Doc ann]
cegisLoop as ss = case pickOne ss of
  Nothing -> ["No solution"]
  Just s -> ["Try:", pretty s] ++
    case runEval prelude $ findCounterExample s as of
    Nothing -> ["Correct!"]
    Just a -> ["Incorrect!", "Counterexample:", pretty a, ""]
      ++ cegisLoop as (pruneExample prelude a ss)

cegis :: String -> Pretty.Doc ann
cegis f = let file = unsafePerformIO $ readFileUtf8 f in
  case lexParse parser file of
    Just defs -> case runRWST (init_ defs) prelude mkGenSt of
      Just (e, st, _) -> case runEval prelude (eval mempty e) of
        Scoped s _ -> Pretty.vsep $
          [ "Assertions:"
          , pretty as
          , "Scope:"
          , pretty s
          -- , "Space (up to depth 2):"
          -- , pretty $ trim 3 ss
          , "Solution 0:"
          , pretty s0
          , "Counterexample:"
          , pretty c0
          ] ++ cegisLoop as ss1
          -- , "Prune holes:"
          -- , pretty $ trim 3 ss0
          -- , "Prune example:"
          -- , pretty $ trim 3 ss1
          -- , "Solution 1:"
          -- , pretty s1
          -- , "Counterexample:"
          -- , pretty c1
          -- , "Prune holes:"
          -- , pretty $ trim 3 ss2
          -- , "Prune example:"
          -- , pretty $ trim 3 ss3
          -- , "Solution 2:"
          -- , pretty s2
          -- , "Counterexample:"
          -- , pretty c2
          -- , "Prune holes:"
          -- , pretty $ trim 3 ss4
          -- , "Prune example:"
          -- , pretty $ trim 3 ss5
          -- , "Solution 3:"
          -- , pretty s3
          -- , "Counterexample:"
          -- , pretty c3
          -- ]
            where
            gs = runReader (gen prelude) st
            as = asserts defs
            ss = withScope s gs
            Just s0 = pickOne ss
            Just c0@(MkAssert x0 _) = runEval prelude $ findCounterExample s0 as
            ss0 = pruneHoles prelude s x0 ss
            -- -- ss0 = ss
            ss1 = pruneExample prelude c0 ss0
            -- Just s1 = pickOne ss1
            -- Just c1@(MkAssert x1 _) = runEval prelude $ findCounterExample s1 as
            -- -- ss2 = pruneHoles prelude s x1 ss1
            -- ss2 = ss1
            -- Just ss3 = Just $ pruneExample prelude c1 ss2
            -- Just s2 = pickOne ss3
            -- Just c2@(MkAssert x2 _) = runEval prelude $ findCounterExample s2 as
            -- -- ss4 = pruneHoles prelude s x2 ss3
            -- ss4 = ss3
            -- Just ss5 = Just $ pruneExample prelude c2 ss4
            -- Just s3 = pickOne ss5
            -- c3 = runEval prelude $ findCounterExample s3 as
            -- Just c3@(MkAssert x3 _) = runEval prelude $ findCounterExample s3 as
        _ -> pretty e

          -- traceShow (pretty e) $
          -- error "Something went wrong in evaluation"
      Nothing -> "Initialization failed"
    Nothing -> error "Could not parse problem"

-- firstExpr :: Term Hole -> Space (Term Hole) -> Maybe (Term Hole)
-- firstExpr e = mfold . fmap (magic . fst) . search . runSearch . explore' e

pickOne :: Space a -> Maybe a
pickOne = mfold . fmap fst . search . runSearch . expl

firstScope :: Scope -> Space (Term Hole) -> Maybe Scope
firstScope m = mfold . fmap fst . search . runSearch . expl . withScope m

foo :: String -> Pretty.Doc ann
foo = pretty . uncurry firstScope . gen'

-- firstN :: Int -> Space a -> [Map Hole a]
-- firstN n = take n . fmap fst . search . runSearch . explore mempty

-- example :: [Map Hole (Term Hole)]
-- example = fmap fst . search . runSearch . explore mempty . generate prelude fs
--   $ Map.singleton 0 $ Goal mempty "Nat"
--   where fs = Map.singleton "f" "(Nat -> Nat) -> Nat"
