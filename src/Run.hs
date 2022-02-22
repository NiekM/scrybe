module Run where

import Types
import Import
import TermGen
import Language
import Language.Prelude
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

composeSketch :: Dec
composeSketch = Dec
  { sig = parseUnsafe parser "(b -> c) -> (a -> b) -> a -> c"
  , def = parseUnsafe parser "{ }"
  }

flipSketch :: Dec
flipSketch = Dec
  { sig = parseUnsafe parser "(a -> b -> c) -> b -> a -> c"
  , def = parseUnsafe parser "{ }"
  }

runSyn :: Syn -> Module -> Dec -> RIO Application ()
runSyn Syn { init, step } m dec = do
  logInfo "Sketch:"
  logInfo ""
  logInfo . display . indent 2 . pretty $ dec
  logInfo ""
  logInfo "Possible refinements:"
  logInfo ""
  case runGenT (init dec) (mkGenState m) of
    Nothing -> logInfo "Something went wrong :("
    Just (x, g) -> do
      let syn = levels $ evalGenT (genTree step x) g
      let xss = takeWhile (not . null) . zip [0 :: Int ..] $ syn
      forM_ xss \(i, xs) -> do
        logInfo $ "Step: " <> fromString (show i)
        forM_ xs (logInfo . display . indent 2 . pretty)

run :: RIO Application ()
run = do
  -- TODO: move these to the test-suite, checking if all generated expressions
  -- type check or perhaps even compare them to exactly what we expect.
  runSyn naive mapPrelude mapSketch
  runSyn naive mapPrelude mapSketch2
  runSyn eta mempty composeSketch
  runSyn eta mempty flipSketch
  logInfo ""
  logInfo "Finished!"
