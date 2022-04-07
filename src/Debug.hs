{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- NOTE: this module is purely for debugging purposes and to load some
-- functions into ghci
module Debug where

import Import
import Language
import Synthesis
import RIO.Text
import System.IO.Unsafe
import Prettyprinter
import qualified RIO.Map as Map
import qualified RIO.Set as Set

fromStr :: Parse a => String -> a
fromStr = fromMaybe (error "Parse failed") . lexParse parser . pack

instance Parse (Expr l v h) => IsString (Expr l v h) where fromString = fromStr
instance IsString Poly where fromString = fromStr

prelude :: Module Void
prelude = let file = unsafePerformIO $ readFileUtf8 "data/prelude.hs" in
  case lexParse parser file of
    Just x -> x
    Nothing -> error "Could not parse prelude"

synSt :: SynState
synSt = mkSynState (fromModule prelude) EtaLong mempty

instance (Pretty a, Pretty b) => Pretty (Map a b) where
  pretty m = align . Prettyprinter.list $ Map.assocs m <&> \(k, x) ->
    pretty k <> ":" <+> align (pretty x)

instance Pretty a => Pretty (Set a) where
  pretty = pretty . Set.toList

instance Pretty HoleCtx where
  pretty (HoleCtx t xs) = parens ("::" <+> pretty t) <> "," <+> pretty xs

tryTC :: Monad m => RWST (Module Void) () TCState m a -> m a
tryTC x = fst <$> runTC x prelude

trySyn :: Monad m => RWST (Module Void) () SynState m a -> m a
trySyn x = fst <$> runSyn x prelude synSt
