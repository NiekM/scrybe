module Run where

import Types
import Import
import TermGen
import Language.Parser
import Language.Prelude
import Language.Syntax
import Algorithms.Naive as Naive
import Prettyprinter
import Data.Tree

mapSketch :: Dec
mapSketch = Dec
  { sig = parseUnsafe parser "(a -> b) -> List a -> List b"
  , def = parseUnsafe parser "\\f. { }"
  }

mapSketch2 :: Dec
mapSketch2 = Dec
  { sig = parseUnsafe parser "(a -> b) -> List a -> List b"
  , def = parseUnsafe parser "\\f. foldr { } { }"
  }

runSyn :: Module -> Dec -> RIO Application ()
runSyn env dec = do
  logInfo "Sketch:"
  logInfo ""
  logInfo . display . indent 2 $ pretty dec
  logInfo ""
  logInfo "Possible refinements:"
  logInfo ""
  case runGenT (fromSketch env dec) env (0, 0) of
    Nothing -> error "OH NO!"
    Just (x, (h, f)) -> do
      let syn = levels $ evalGenT (genTree step x) env (h, f)
      let xss = takeWhile (not . null) . zip [0 :: Int ..] $ syn
      forM_ xss \(i, xs) -> do
        logInfo $ "Step: " <> fromString (show i)
        forM_ xs (logInfo . display . indent 2 . pretty . expr)

run :: RIO Application ()
run = do
  -- TODO: move these to the test-suite, checking if all generated expressions
  -- type check or perhaps even compare them to exactly what we expect.
  runSyn prelude mapSketch
  runSyn prelude mapSketch2
  logInfo ""
  logInfo "Finished!"
