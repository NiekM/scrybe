{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- NOTE: this module is purely for debugging purposes and to load some
-- functions and instances into ghci
module Debug where

import Import
import Language hiding (free)
import qualified RIO.Text as T
import System.IO.Unsafe
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

gen' :: String -> (Scope, Space Info (Term Hole))
gen' f = let file = unsafePerformIO $ readFileUtf8 f in
  case lexParse parser file of
    Just x -> case runRWST (init_ x) prelude mkGenSt of
      Just (e, st, _) -> case runEval prelude (eval mempty e) of
        Scoped s _ -> (s, runReader (gen prelude) st)
        _ -> error "Something went really wrong in evaluation"
      Nothing -> error "Initialization failed"
    Nothing -> error "Could not parse problem"

cegisLoop :: [Assert] -> Scope -> Space Info Scope -> [Pretty.Doc ann]
cegisLoop [] _ _ = ["No assertions..."]
cegisLoop as@(MkAssert e _ : _) m ss =
  case pickOne m e ss of
  Nothing -> ["No solution"]
  Just (s, d) -> ["Try:", pretty s, ""] ++
    case runEval prelude $ findCounterExample s as of
    Nothing -> ["Correct!", pretty (fromIntegral d :: Int)]
    Just a -> ["Incorrect!", "Counterexample:", pretty a, ""]
      ++ cegisLoop as m (pruneExample prelude a ss)

cegis :: String -> Pretty.Doc ann
cegis f = case runRWST (init_ problem) prelude mkGenSt of
  Just (e, st, _) -> case runEval prelude (eval mempty e) of
    Scoped s _ -> Pretty.vsep $
      [ ""
      , Pretty.indent 2 $ pretty problem
      , ""
      , "Scope:"
      , ""
      , pretty s
      , ""
      ] ++ cegisLoop as s ss
      where
        gs = runReader (gen prelude) st
        as = asserts problem
        fs = forbid (view envForbidden prelude) gs
        ss = withScope s fs
    _ -> pretty e
  Nothing -> "Initialization failed"
  where
    file = unsafePerformIO $ readFileUtf8 f
    problem = case lexParse parser file of
      Nothing -> error "Could not parse problem"
      Just p -> p

pickOne :: Scope -> Term Hole -> Space Info Scope -> Maybe (Scope, Dist)
pickOne m e = mfold . search . runSearch . expl . pruneHoles prelude m e
