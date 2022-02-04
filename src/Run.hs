module Run (run) where

import Import
import TermGen
import Language.Parser
import Language.Prelude
import Language.Syntax
import qualified RIO.Map as Map

mapSketch :: Term (Type Void)
mapSketch = parseUnsafe "\\f :: a -> b. {List a -> List b}"

mapSketch2 :: Term (Type Void)
mapSketch2 = parseUnsafe
  "\\f :: a -> b. foldr {a -> List b -> List b} {List b}"

runSyn :: [(Var, Type Hole)] -> Term (Type Void) -> RIO App ()
runSyn ctx body = do
  let env = Map.fromList ctx
  logInfo "Sketch:"
  logInfo ""
  logInfo . display . indent 2 $ pretty body
  logInfo ""
  logInfo "Possible refinements:"
  logInfo ""
  let syn = synthesize env body
  let xss = takeWhile (not . null) . zip [0 :: Int ..] $ syn
  forM_ xss \(i, xs) -> do
    logInfo $ "Step: " <> fromString (show i)
    forM_ xs (logInfo . display . indent 2 . pretty . expr)

run :: RIO App ()
run = do
  runSyn prelude mapSketch
  runSyn (filter (\(n, _) -> n /= "foldr") prelude) mapSketch2
  logInfo ""
  logInfo "Finished!"
