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

cegisLoop :: [Assert] -> Scope -> Space Scope -> [Pretty.Doc ann]
cegisLoop [] _ _ = ["No assertions..."]
cegisLoop as@(MkAssert e _ : _) m ss = --pretty (trim 3 ss) :
  case pickOne m e ss of
  Nothing -> ["No solution"]
  Just s -> ["Try:", pretty s] ++
    case runEval prelude $ findCounterExample s as of
    Nothing -> ["Correct!"]
    Just a -> ["Incorrect!", "Counterexample:", pretty a, ""]
      -- ++ cegisLoop as m (pruneExample prelude a . pruneHoles prelude m e $ ss)
      ++ cegisLoop as m (pruneExample prelude a ss)

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
          -- , "Solution 0:"
          -- , pretty s0
          -- , "Counterexample:"
          -- , pretty c0
          ] ++ cegisLoop as s ss
            where
            gs = runReader (gen prelude) st
            as = asserts defs
            ss = withScope s gs
            -- Just s0 = pickOne ss
            -- Just c0@(MkAssert x0 _) = runEval prelude $ findCounterExample s0 as
            -- ss0 = pruneHoles prelude s x0 ss
            -- ss1 = pruneExample prelude c0 ss0
        _ -> pretty e

          -- traceShow (pretty e) $
          -- error "Something went wrong in evaluation"
      Nothing -> "Initialization failed"
    Nothing -> error "Could not parse problem"

pickOne :: Scope -> Term Hole -> Space Scope -> Maybe Scope
pickOne m e = mfold . fmap fst . search . runSearch . expl . pruneHoles prelude m e

-- pickOne :: Space a -> Maybe a
-- pickOne = mfold . fmap fst . search . runSearch . expl
