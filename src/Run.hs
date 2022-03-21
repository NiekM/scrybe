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
  [ (Func "nil", Just 1)
  , (Func "cons", Just 1)
  , (Func "foldr", Just 1)
  ]

type Insts = [(Var, [Map Var (Type Void)])]

mapInsts :: Insts
mapInsts =
  [ ("nil", [mempty])
  , ("cons", [mempty])
  , ("foldr", [mempty])
  ]

mapConcepts2 :: MultiSet Concept
mapConcepts2 = Map.fromList
  [ (Func "nil", Just 1)
  , (Func "cons", Just 1)
  ]

mapInsts2 :: Insts
mapInsts2 =
  [ ("nil", [mempty])
  , ("cons", [mempty])
  ]

mapConcepts3 :: MultiSet Concept
mapConcepts3 = Map.fromList
  [ (Func "nil", Just 1)
  , (Func "cons", Just 1)
  , (Func "foldr", Just 1)
  , (Func "compose", Just 1)
  ]

mapInsts3 :: Insts
mapInsts3 =
  [ ("nil", [mempty])
  , ("cons", [mempty])
  , ("foldr", [mempty])
  -- NOTE: combinators such as `compose` blow up the search space, unless they
  -- are instantiated to very specific types, in which case they might actually
  -- speed up the synthesis in a pointfree setting.
  , ("compose", [Map.fromList
    [ ("a", p "A")
    , ("b", p "B")
    , ("c", p "List B -> List B")
    ]])
  ]

composeSketch :: Sketch
composeSketch = p "{ } :: (B -> C) -> (A -> B) -> A -> C"

flipSketch :: Sketch
flipSketch = p "{ } :: (A -> B -> C) -> B -> A -> C"

foldrSketch :: Sketch
foldrSketch = p "{ } :: (A -> B -> B) -> B -> List A -> B"

foldrConcepts :: MultiSet Concept
foldrConcepts = Map.fromList
  [ (Func "rec", Just 1)
  , (Func "elimList", Just 1)
  ]

fromModule :: Module -> Environment
fromModule m = flip Map.mapWithKey (functions m)
  \x (_, t) -> (t, Set.singleton $ Func x)

-- TODO: I guess different instantiations should be considered different
-- 'concepts'
instantiations :: Insts -> Environment -> Environment
instantiations xs e = Map.fromList do
  (x, as) <- xs
  case Map.lookup x e of
    Just (t, cs) -> do
      a <- as
      return (x, (instantiate a t, cs))
    Nothing -> []

runSyn :: String -> Technique -> MultiSet Concept -> Insts -> Sketch ->
  RIO Application ()
runSyn file t c is dec = do
  m <- p <$> readFileUtf8 file
  let env' = instantiations is . restrict (Map.keysSet c) $ fromModule m
  -- let env' = restrict (Map.keysSet c) $ fromModule m
  logInfo "Sketch:"
  logInfo ""
  logInfo . display . indent 2 . pretty $ dec
  logInfo ""
  logInfo $ "Technique: " <> displayShow t
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
  runSyn prelude EtaLong mempty [] composeSketch
  runSyn prelude EtaLong mempty [] flipSketch
  runSyn prelude EtaLong mapConcepts mapInsts mapSketch
  runSyn prelude EtaLong mapConcepts2 mapInsts2 mapSketch2
  runSyn prelude PointFree mapConcepts3 mapInsts3 mapSketch
  -- runSyn prelude EtaLong foldrConcepts foldrSketch
  logInfo "Finished!"
