module Run where

import Types
import Import
import TermGen
import Language.Parser
import Language.Prelude
import Language.Syntax
import Algorithms.Naive as Naive
import Prettyprinter

mapSketch :: Dec
mapSketch = Dec
  { sig = parseUnsafe parser "(a -> b) -> List a -> List b"
  , def = parseUnsafe parser "\\f. {0}"
  }

mapSketch2 :: Dec
mapSketch2 = Dec
  { sig = parseUnsafe parser "(a -> b) -> List a -> List b"
  , def = parseUnsafe parser "\\f. foldr {0} {1}"
  }

runSyn :: Module -> Dec -> RIO Application ()
runSyn env body = do
  logInfo "Sketch:"
  logInfo ""
  logInfo . display . indent 2 $ pretty body
  logInfo ""
  logInfo "Possible refinements:"
  logInfo ""
  let syn = synthesize @Naive.GenSt env body
  let xss = takeWhile (not . null) . zip [0 :: Int ..] $ syn
  forM_ xss \(i, xs) -> do
    logInfo $ "Step: " <> fromString (show i)
    forM_ xs (logInfo . display . indent 2 . pretty . result)

run :: RIO Application ()
run = do
  runSyn prelude mapSketch
  runSyn prelude mapSketch2
  logInfo ""
  logInfo "Finished!"
