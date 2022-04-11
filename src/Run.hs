module Run where

import Types hiding (Options)
import Import
import Language
import Synthesis
import Prettyprinter
import Data.Tree
import qualified RIO.Map as Map
import System.IO

syntax :: Parse a => Text -> RIO Application a
syntax t = case lexParse parser t of
  Nothing -> fail "Parsing failed"
  Just y -> return y

climb :: (a ~ (Hole, Ref, Term Var Hole, Refs)) =>
  [Tree a] -> RIO Application [a]
climb xs = do
  let n = (2+) . length . show $ length xs
  for_ (zip [0 :: Int ..] xs) \(i, Node (h, Ref e th _, _, _) _) ->
    logInfo . display $ Prettyprinter.fill n (pretty i <> ":") <+>
      pretty h <+> "|->" <+> pretty e <> if null th then "" else " ---" <+>
        tupled (Map.assocs th <&> \(k, x) -> pretty k <+> "|->" <> pretty x)
  l <- liftIO getLine
  case readMaybe l >>= \i -> preview (ix i) xs of
    Nothing -> logInfo "Done..." >> return []
    Just (Node y ys) -> do
      logInfo ""
      logInfo . display . pretty $ view _3 y
      logInfo ""
      (y:) <$> climb ys

readFiles :: String -> String -> String ->
  RIO Application (Module Void, Sketch, Ann Type 'Term Var Hole)
readFiles file sketch model = do
  m <- syntax =<< readFileUtf8 ("data/" <> file <> ".hs")
  sk@(Sketch _ t _) <- syntax =<< readFileUtf8
    ("data/examples/sketch/" <> sketch <> ".hs")
  Sketch _ u b <- syntax =<< readFileUtf8
    ("data/examples/model/" <> model <> ".hs")
  case alphaEq t u of
    Nothing -> fail "Model and sketch signature do not match."
    Just _ -> return ()
  (a, _, _) <- evalTC (check b t) m
  return (m, sk, a)

interactive :: String -> String -> String -> Technique -> RIO Application ()
interactive file sketch model t = do
  (m, sk, a) <- readFiles file sketch model
  let (env', c) = fromSketch m a
  logInfo "Sketch:"
  logInfo ""
  logInfo . display . indent 2 . pretty $ sk
  logInfo ""
  logInfo $ "Technique: " <> displayShow t
  let xs = runSyn (init' sk) m (mkSynState env' t c)
  _ <- climb $ xs <&> \(x, g) -> view _1 <$> runSyn (search step' x) m g
  logInfo ""

synth :: String -> String -> String -> Technique -> RIO Application ()
synth file sketch model t = do
  (m, sk, a) <- readFiles file sketch model
  let (env', c) = fromSketch m a
  logInfo "Sketch:"
  logInfo ""
  logInfo . display . indent 2 . pretty $ sk
  logInfo ""
  logInfo $ "Technique: " <> displayShow t
  case runSyn (init sk) m (mkSynState env' t c) of
    Nothing -> logInfo "Something went wrong :("
    Just (y, g) -> do
      let syn = levels $ runSyn (search step y) m g
      let xss = take 10 . takeWhile (not . null) . zip [0 :: Int ..] $ syn
      forM_ xss \(i, xs) -> do
        logInfo $ "Step: " <> fromString (show i)
        forM_ xs \(e, _s) -> do
          logInfo . display . indent 2 . pretty $ e
          -- forM_ (Map.assocs $ _s ^. holeCtxs) \(h, HoleCtx goal local) -> do
          --   logInfo . display . indent 4 .
          --     ((pretty h <+> "::" <+> pretty goal <+> colon) <+>) . align $
          --     vsep (fmap pretty . catMaybes $
          --       (\(a, b) -> Map.lookup b (_s ^. variables) <&>
          --         ((a,) . (\(Variable _ u _ _) -> u)))
          --           <$> Map.assocs local)
  logInfo ""

synth' :: Technique -> String -> RIO Application ()
synth' t s = synth "prelude" s s t

run :: RIO Application ()
run = do
  -- TODO: move these to the test-suite, checking if all generated expressions
  -- type check or perhaps even compare them to exactly what we expect.
  traverse_ (synth' EtaLong)
    [ "compose"
    , "flip"
    -- TODO: map as unfold?
    -- map f = unfoldr \case [] -> Nothing; x:xs -> Just (f x, xs)
    , "map"
    -- TODO: add simple testing to differentiate stutter from idList
    , "stutter"
    -- TODO: allow ignoring variables
    , "length"
    -- , "append"
    -- , "replicate"
    -- TODO: restrict search space more, e.g. by only allowing pattern matching
    -- on variables
    -- , "compareNat"
    -- , "takeWhile"
    ]
  -- runSyn' "map_pointfree" PointFree
  interactive "prelude" "length" "length" EtaLong
