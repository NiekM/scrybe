module Run where

import Options
import Import
import Language hiding (indent, Assert)
import Synthesis
import Prettyprinter hiding (fill)
import Control.Monad.Heap
import qualified RIO.Map as Map

parseDefs :: Parse a => String -> RIO Application (Defs a)
parseDefs s = do
  f <- readFileUtf8 s
  case lexParse parser f of
    Nothing -> fail "Could not parse file."
    Just y -> return y

synthesize :: String -> Options -> RIO Application ()
synthesize file opts = do
  prelude <- fromDefs . recDefs <$> parseDefs (view optPrelude opts)
  logDebug "Parsed prelude..."
  problem <- parseDefs file
  logDebug "Parsed problem..."
  logInfo ""
  logInfo . display . indent 2 $ pretty problem
  logInfo ""
  let syn = best . runNondet . runSynth prelude $ synth problem
  let t = view optTimeout opts * 1000
  res <- timeout t $ syn `seq` return syn
  case res of
    Nothing -> logInfo "Synthesis failed: Timeout"
    Just Nothing -> logInfo "Synthesis failed: Exhaustive"
    Just (Just (n, hf)) -> do
      logInfo "Solution found!"
      logInfo ""
      logInfo . display $ "Depth:" <+> pretty (fromIntegral n :: Int)
      logInfo ""
      logInfo . display . nest 2 . vsep $ "Hole fillings:" :
        (Map.assocs hf <&> \(h, e) -> pretty h <> ":" <+> pretty e)
      logInfo ""
      logInfo . display . nest 2 . vsep $ "Result:" :
        ( relBinds problem <&> \(MkBinding x e) ->
          pretty $ MkBinding x (fill (normalizeFilling hf) e)
        )
      logInfo ""

live :: String -> Options -> RIO Application ()
live input opts = do
  prelude <- fromDefs . recDefs <$> parseDefs (view optPrelude opts)
  logDebug "Parsed prelude..."
  case lexParse parser $ fromString input of
    Nothing -> fail "Parse failed"
    Just expr -> case runTC (infer mempty expr) mkFreshState prelude of
      Nothing -> fail "Type check failed"
      Just ((x, _), _) -> do
        let r = runEval prelude (eval mempty $ over holes fst $ strip x)
        logInfo . display . pretty $ r

assert :: String -> Options -> RIO Application ()
assert input opts = do
  prelude <- fromDefs . recDefs <$> parseDefs (view optPrelude opts)
  logDebug "Parsed prelude..."
  case lexParse parser $ fromString input of
    Nothing -> fail "Parse failed"
    Just (MkAssert e ex) -> case runTC (infer mempty e) mkFreshState prelude of
      Nothing -> fail "Type check failed"
      Just ((x, _), _) -> do
        let r = runEval prelude (eval mempty $ over holes fst $ strip x)
        case runUneval prelude 1000 $ uneval r $ toEx ex of
          Nothing -> fail "Uneval failed: out of fuel"
          Just cs -> case mergeConstraints <$> dnf cs of
            [] -> logInfo "Uneval failed: structural constraint conflict"
            ds -> case catMaybes ds of
              []  -> logInfo "Uneval failed: inconsistent constraint"
              [d] -> logInfo . display $ displayWorld d
              ds' -> do
                logInfo $ display (length ds) <> " possible worlds"
                for_ ds' $ logInfo . display . displayWorld

displayWorld :: Constraints -> Doc ann
displayWorld w = ("-" <+>) . align . vsep $ Map.assocs w <&> \(h, c) ->
  pretty h <> ":" <+> displayConstraint c

displayConstraint :: Constraint -> Doc ann
displayConstraint c = align . vsep $ Map.assocs c <&> \(k, ex) ->
  if null k then displayEx ex else pretty k <+> "|-" <+> displayEx ex

displayEx :: Ex -> Doc ann
displayEx ex = case fromEx ex of
  []  -> "T"
  [e] -> pretty e
  es  -> align . vsep $ pretty <$> es

run :: RIO Application ()
run = do
  opts <- view optionsL
  cmd <- view commandL
  case cmd of
    Synth f -> synthesize f opts
    Live e -> live e opts
    Assert a -> assert a opts
