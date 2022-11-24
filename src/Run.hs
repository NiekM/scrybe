module Run where

import Options
import Import
import Utils.Weighted
import Language hiding (Assert)
import Synthesis
import Prettyprinter hiding (fill)
import Control.Monad.Heap
import qualified RIO.Map as Map

parseDefs :: (MonadIO m, MonadFail m, Parse a) => String -> m (Defs a)
parseDefs s = do
  f <- readFileUtf8 s
  case lexParse parser f of
    Nothing -> fail "Could not parse file."
    Just y -> return y

timed :: (HasOptions r, NFData a) => a -> RIO r (Maybe a)
timed x = do
  t <- (1000*) . view (optionsL . optTimeout) <$> ask
  timeout t $ x `deepseq` return x

getPrelude :: RIO Application Env
getPrelude = do
  file <- view (optionsL . optPrelude) <$> ask
  prelude <- fromDefs . recDefs <$> parseDefs file
  logDebug "Parsed prelude..."
  return prelude

synthesize :: String -> SynOptions -> RIO Application ()
synthesize file opts = do
  prelude <- getPrelude
  problem <- parseDefs file
  logDebug "Parsed problem..."
  logInfo ""
  logInfo . display . indent 2 $ pretty problem
  logInfo ""
  let syn = best . runSearch . runSynth opts prelude $ synth problem
  timed syn >>= \case
    Nothing -> logInfo "Synthesis failed: Timeout"
    Just Nothing -> logInfo "Synthesis failed: Exhaustive"
    Just (Just (n, (expr, hf))) -> do
      logInfo "Solution found!"
      logInfo ""
      logInfo . display $ "Depth:" <+> pretty (fromIntegral n :: Int)
      logInfo ""
      logInfo . display . nest 2 . vsep $ "Hole fillings:" :
        (Map.assocs hf <&> \(h, e) -> pretty h <> ":" <+> pretty e)
      logInfo ""
      logInfo . display . nest 2 . vsep $ "Result:" :
        ( relevant expr <&> \(MkBinding x e) ->
          pretty $ MkBinding x (fill (normalizeFilling hf) e)
        )
      logInfo ""

live :: String -> RIO Application ()
live input = do
  prelude <- getPrelude
  case lexParse parser $ fromString input of
    Nothing -> logInfo "Parse failed"
    Just expr -> case evalInfer (infer mempty expr) 0 prelude of
      Nothing -> logInfo "Type check failed"
      Just _ -> do
        let liv = runEval prelude (eval mempty expr)
        timed liv >>= \case
          Nothing -> logInfo "Evaluation failed: Timeout"
          Just r -> logInfo . display . pretty $ r

assert :: String -> RIO Application ()
assert input = do
  prelude <- getPrelude
  case lexParse parser $ fromString input of
    Nothing -> logInfo "Parse failed"
    Just (MkAssert e ex) -> case evalInfer (infer mempty e) 0 prelude of
      Nothing -> logInfo "Type check failed"
      Just _ -> do
        let r = runEval prelude (eval mempty e)
        let ass = runUneval prelude 1000 $ uneval r $ toEx ex
        timed ass >>= \case
          Nothing -> logInfo "Assertion failed: Timeout"
          Just Nothing -> logInfo "Out of fuel"
          Just (Just cs) -> case mergeConstraints <$> dnf cs of
            [] -> logInfo "Assertion failed: structural constraint conflict"
            ds -> case catMaybes ds of
              []  -> logInfo "Assertion failed: inconsistent constraint"
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
run = view commandL >>= \case
  Synth f opts -> synthesize f opts
  Live e -> live e
  Assert a -> assert a
