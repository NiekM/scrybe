module Run where

import Types
import Import
import TermGen
import Language
import Language.Prelude
import Algorithms.Naive as Naive
import Prettyprinter
import Data.Tree
import qualified RIO.Map as Map

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
  logInfo . display . indent 2 . pretty $ dec
  logInfo ""
  logInfo "Possible refinements:"
  logInfo ""
  case runGenT (fromSketch dec) env emptyGenState of
    Nothing -> logInfo "Something went wrong :("
    Just (x, g) -> do
      let syn = levels $ evalGenT (genTree step x) env g
      let xss = takeWhile (not . null) . zip [0 :: Int ..] $ syn
      forM_ xss \(i, xs) -> do
        logInfo $ "Step: " <> fromString (show i)
        forM_ xs (logInfo . display . indent 2 . pretty)

eta :: Module -> Dec -> RIO Application ()
eta env dec = do
  logInfo "Input:"
  logInfo ""
  logInfo . display . indent 2 $ pretty dec
  logInfo ""
  case runGenT (fromSketch dec >>= etaExpand) env emptyGenState of
    Nothing -> logInfo "Something went wrong :("
    Just (x, g) -> do
      logInfo "Eta expanded:"
      logInfo ""
      logInfo . display . indent 2 . pretty $ x
      forM_ (Map.assocs . ctxs $ g) \(i, (t, ctx)) -> do
        logInfo ""
        logInfo . display . indent 2 $ pretty (Hole i) <+> "::" <+> pretty t
        forM_ (Map.assocs ctx) \(v, u) ->
          logInfo . display . indent 4 $ pretty v <+> "::" <+> pretty u

run :: RIO Application ()
run = do
  -- TODO: move these to the test-suite, checking if all generated expressions
  -- type check or perhaps even compare them to exactly what we expect.
  runSyn prelude mapSketch
  runSyn prelude mapSketch2
  eta prelude mapSketch
  eta prelude mapSketch2
  logInfo ""
  logInfo "Finished!"
