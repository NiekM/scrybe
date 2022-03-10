module Run where

import Types hiding (Options)
import Import
import TermGen
import Language
import Language.Prelude
import Algorithms.Naive as Naive
import Prettyprinter
import Data.Tree
import qualified RIO.Map as Map
import qualified RIO.Set as Set

p :: Parse a => Text -> a
p = parseUnsafe parser

mapSketch :: Sketch
mapSketch = p "\\f. { } :: (a -> b) -> List a -> List b"

mapSketch2 :: Sketch
mapSketch2 = p "\\f. foldr { } { } :: (a -> b) -> List a -> List b"

mapConcepts :: MultiSet Concept
mapConcepts = Map.fromList
  [ (Function "nil", Just 1)
  , (Function "cons", Just 1)
  , (Function "foldr", Just 1)
  ]

mapConcepts2 :: MultiSet Concept
mapConcepts2 = Map.fromList
  [ (Function "nil", Just 1)
  , (Function "cons", Just 1)
  ]

mapConcepts3 :: MultiSet Concept
mapConcepts3 = Map.fromList
  [ (Function "nil", Just 1)
  , (Function "cons", Just 1)
  , (Function "foldr", Just 1)
  , (Function "compose", Just 1)
  ]

composeSketch :: Sketch
composeSketch = p "{ } :: (b -> c) -> (a -> b) -> a -> c"

flipSketch :: Sketch
flipSketch = p "{ } :: (a -> b -> c) -> b -> a -> c"

foldrSketch :: Sketch
foldrSketch = p "{ } :: (a -> b -> b) -> b -> List a -> b"

foldrConcepts :: MultiSet Concept
foldrConcepts = Map.fromList
  [ (Function "rec", Just 1)
  , (Function "elimList", Just 1)
  ]

runSyn :: Module -> Technique -> MultiSet Concept
  -> Sketch -> RIO Application ()
runSyn m t c dec = do
  let env' = restrict (Map.keysSet c) $ Map.assocs (functions m) <&>
        \(x, u) -> (x, u, Set.singleton $ Function x)
  logInfo "Sketch:"
  logInfo ""
  logInfo . display . indent 2 . pretty $ dec
  logInfo ""
  logInfo "Possible refinements:"
  logInfo ""
  case runGenT (init dec) m (mkGenState env' t c) of
    Nothing -> logInfo "Something went wrong :("
    Just (x, g) -> do
      let syn = levels $ runGenT (genTree step x) m g
      let xss = take 11 . takeWhile (not . null) . zip [0 :: Int ..] $ syn
      forM_ xss \(i, xs) -> do
        logInfo $ "Step: " <> fromString (show i)
        forM_ xs \(e, _s) -> do
          logInfo . display . indent 2 . pretty $ e
          forM_ (Map.assocs $ _s ^. holeCtxs) \(h, HoleCtx {goal,local}) -> do
            logInfo . display . indent 4 .
              ((pretty h <+> "::" <+> pretty goal <+> colon) <+>) . align $
              vsep (fmap pretty . catMaybes $
                (\(a, b) -> Map.lookup b (_s ^. variables) <&>
                  ((a,) . (\(Variable _ u _ _) -> u)))
                    <$> Map.assocs local)
  logInfo ""

pre :: Module
pre = Module mempty prelude

run :: RIO Application ()
run = do
  -- TODO: move these to the test-suite, checking if all generated expressions
  -- type check or perhaps even compare them to exactly what we expect.
  runSyn pre EtaLong mempty composeSketch
  runSyn pre EtaLong mempty flipSketch
  runSyn pre EtaLong mapConcepts mapSketch
  -- runSyn pre EtaLong mapConcepts2 mapSketch2
  -- runSyn pre PointFree mapConcepts3 mapSketch
  -- runSyn pre EtaLong foldrConcepts foldrSketch
  logInfo "Finished!"
