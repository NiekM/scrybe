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

mapSketch :: Dec
mapSketch = p "\\f. { } :: (a -> b) -> List a -> List b"

mapSketch2 :: Dec
mapSketch2 = p "\\f. foldr { } { } :: (a -> b) -> List a -> List b"

mapEnv :: Environment
mapEnv =
  [ ("true", p "Bool", Set.singleton $ Function "true")
  , ("false", p "Bool", Set.singleton $ Function "false")
  , ("nil", p "List {0}", Set.singleton $ Function "nil")
  , ("cons", p "{0} -> List {0} -> List {0}", Set.singleton $ Function "cons")
  , ("foldr", p "({0} -> {1} -> {1}) -> {1} -> List {0} -> {1}"
    , Set.singleton $ Function "foldr")
  , ("compose", p "({1} -> {2}) -> ({0} -> {1}) -> ({0} -> {2})"
    , Set.singleton $ Function "compose")
  ]

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

composeSketch :: Dec
composeSketch = p "{ } :: (b -> c) -> (a -> b) -> a -> c"

flipSketch :: Dec
flipSketch = p "{ } :: (a -> b -> c) -> b -> a -> c"

runSyn :: Syn -> Module -> Environment -> Technique -> MultiSet Concept
  -> Dec -> RIO Application ()
runSyn Syn { init, step } m env t c dec = do
  logInfo "Sketch:"
  logInfo ""
  logInfo . display . indent 2 . pretty $ dec
  logInfo ""
  logInfo "Possible refinements:"
  logInfo ""
  case runGenT (init dec) m (mkGenState env t c) of
    Nothing -> logInfo "Something went wrong :("
    Just (x, g) -> do
      let syn = levels $ runGenT (genTree step x) m g
      let xss = take 12 . takeWhile (not . null) . zip [0 :: Int ..] $ syn
      forM_ xss \(i, xs) -> do
        logInfo $ "Step: " <> fromString (show i)
        forM_ xs \(e, s) -> do
          logInfo . display . indent 2 . pretty $ e
          -- forM_ (Map.assocs $ s ^. holeCtxs) \(i, HoleCtx {goal,local}) -> do
          --   logInfo . display . indent 4 .
          --     ((pretty i <+> "::" <+> pretty goal <+> colon) <+>) . align $
          --     vsep (pretty <$> Map.assocs local)
  logInfo ""

run :: RIO Application ()
run = do
  -- TODO: move these to the test-suite, checking if all generated expressions
  -- type check or perhaps even compare them to exactly what we expect.
  runSyn synth mapPrelude mempty EtaLong mempty composeSketch
  runSyn synth mapPrelude mempty EtaLong mempty flipSketch
  runSyn synth mapPrelude (restrict (Map.keysSet mapConcepts) mapEnv)
    EtaLong mapConcepts mapSketch
  runSyn synth mapPrelude (restrict (Map.keysSet mapConcepts2) mapEnv)
    EtaLong mapConcepts2 mapSketch2
  runSyn synth mapPrelude (restrict (Map.keysSet mapConcepts3) mapEnv)
    PointFree mapConcepts3 mapSketch
  logInfo "Finished!"
