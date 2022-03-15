module Run where

import Types hiding (Options)
import Import
import TermGen
import Language
import Synthesis
import Prettyprinter
import Data.Tree
import qualified RIO.Map as Map
import qualified RIO.Set as Set

p :: Parse a => Text -> a
p = parseUnsafe parser

-- TODO: use polytypes and skolemnization

mapSketch :: Sketch
mapSketch = p "\\f -> { } :: (A -> B) -> List A -> List B"

mapSketch2 :: Sketch
mapSketch2 = p "\\f -> foldr { } { } :: (A -> B) -> List A -> List B"

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
composeSketch = p "{ } :: (B -> C) -> (A -> B) -> A -> C"

flipSketch :: Sketch
flipSketch = p "{ } :: (A -> B -> C) -> B -> A -> C"

foldrSketch :: Sketch
foldrSketch = p "{ } :: (A -> B -> B) -> B -> List A -> B"

foldrConcepts :: MultiSet Concept
foldrConcepts = Map.fromList
  [ (Function "rec", Just 1)
  , (Function "elimList", Just 1)
  ]

runSyn :: String -> Technique -> MultiSet Concept
  -> Sketch -> RIO Application ()
runSyn file t c dec = do
  m <- p <$> readFileUtf8 file
  let env' = restrict (Map.keysSet c) $ Map.assocs (functions m) <&>
        \(x, (_, u)) -> (x, u, Set.singleton $ Function x)
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
      let xss = take 9 . takeWhile (not . null) . zip [0 :: Int ..] $ syn
      forM_ xss \(i, xs) -> do
        logInfo $ "Step: " <> fromString (show i)
        forM_ xs \(e, _s) -> do
          logInfo . display . indent 2 . pretty $ e
          -- forM_ (Map.assocs $ _s ^. holeCtxs) \(h, HoleCtx {goal,local}) -> do
          --   logInfo . display . indent 4 .
          --     ((pretty h <+> "::" <+> pretty goal <+> colon) <+>) . align $
          --     vsep (fmap pretty . catMaybes $
          --       (\(a, b) -> Map.lookup b (_s ^. variables) <&>
          --         ((a,) . (\(Variable _ u _ _) -> u)))
          --           <$> Map.assocs local)
  logInfo ""

prelude :: String
prelude = "data/Prelude.hs"

run :: RIO Application ()
run = do
  -- TODO: move these to the test-suite, checking if all generated expressions
  -- type check or perhaps even compare them to exactly what we expect.
  runSyn prelude EtaLong mempty composeSketch
  runSyn prelude EtaLong mempty flipSketch
  runSyn prelude EtaLong mapConcepts mapSketch
  -- runSyn prelude EtaLong mapConcepts2 mapSketch2
  -- runSyn prelude PointFree mapConcepts3 mapSketch
  -- runSyn prelude EtaLong foldrConcepts foldrSketch
  logInfo "Finished!"
