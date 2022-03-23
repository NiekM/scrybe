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

syntax :: Parse a => Text -> RIO Application a
syntax t = case lexParse parser t of
  Nothing -> logError "Parsing failed" >> exitFailure
  Just y -> return y

runSyn :: String -> String -> String -> Technique -> RIO Application ()
runSyn file sketch pre t = do
  m <- syntax =<< readFileUtf8 ("data/" <> file <> ".hs")
  sk <- syntax =<< readFileUtf8
    ("data/examples/sketch/" <> sketch <> ".hs")
  Sigs ss <- syntax =<< readFileUtf8
    ("data/examples/preludes/" <> pre <> ".hs")
  let c = Map.fromList $ ss <&> \(MkSignature x _) -> (Func x, Just 1)
  let env' = Map.fromList $ ss <&> \(MkSignature x u) ->
        (x, (u, Set.singleton $ Func x))
  -- let env' = instantiations is . restrict (Map.keysSet c) $ fromModule m
  logInfo "Sketch:"
  logInfo ""
  logInfo . display . indent 2 . pretty $ sk
  logInfo ""
  logInfo $ "Technique: " <> displayShow t
  logInfo ""
  logInfo "Possible refinements:"
  logInfo ""
  case runGenT (init sk) m (mkGenState env' t c) of
    Nothing -> logInfo "Something went wrong :("
    Just (x, g) -> do
      let syn = levels $ runGenT (genTree step x) m g
      let xss = take 10 . takeWhile (not . null) . zip [0 :: Int ..] $ syn
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

run :: RIO Application ()
run = do
  -- TODO: move these to the test-suite, checking if all generated expressions
  -- type check or perhaps even compare them to exactly what we expect.
  runSyn "prelude" "compose" "combinator" EtaLong
  runSyn "prelude" "flip" "combinator" EtaLong
  runSyn "prelude" "map_eta" "map_eta" EtaLong
  runSyn "prelude" "map_pointfree" "map_pointfree" PointFree
  -- TODO: allow Nil to be used twice for stutter
  -- TODO: add simple testing to differentiate stutter from idList
  runSyn "prelude" "stutter" "stutter" EtaLong
  -- TODO: allow ignoring variables
  runSyn "prelude" "length" "length" EtaLong
  logInfo "Finished!"
