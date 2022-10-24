module Run where

import Types
import Import
import Language
import Synthesis
import Prettyprinter hiding (fill)
import Control.Monad.Heap

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
  logInfo . display $ pretty problem
  logInfo ""
  case best . runNondet . runSynth prelude $ synth problem of
    Nothing -> logInfo "Synthesis failed"
    Just (n, hf) -> do
      logInfo "Synthesis succeeded!"
      logInfo . display $ "Depth:" <+> pretty (fromIntegral n :: Int)
      logInfo . display $ "Hole fillings:" <+> pretty hf
      logInfo . display $ "Result:" <+> pretty (relBinds problem <&>
        \(MkBinding x e) -> MkBinding x (fill (normalizeFilling hf) e))
