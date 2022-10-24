module Run where

import Types
import Import
import Language hiding (indent)
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

run :: RIO Application ()
run = do
  opts <- view optionsL
  prelude <- fromDefs . recDefs <$> parseDefs (view optPrelude opts)
  logInfo "Parsed prelude..."
  problem <- parseDefs $ view optInput opts
  logInfo "Parsed problem..."
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
