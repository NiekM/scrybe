module Run (run) where

import Types
import Import
import TermGen
import Language.Parser
import Language.Prelude
import Language.Syntax
import qualified RIO.Map as Map
import Algorithms.Naive
import Prettyprinter

mapSketch :: Term (Type Void)
mapSketch = parseUnsafe parser "\\f :: a -> b. {List a -> List b}"

mapSketch2 :: Term (Type Void)
mapSketch2 = parseUnsafe parser
  "\\f :: a -> b. foldr {a -> List b -> List b} {List b}"

runSyn :: Module -> Term (Type Void) -> RIO Application ()
runSyn env body = do
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

run :: RIO Application ()
run = do
  runSyn prelude mapSketch
  runSyn prelude { vars = Map.delete "foldr" $ vars prelude } mapSketch2
  logInfo ""
  logInfo "Finished!"
