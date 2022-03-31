{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- NOTE: this module is purely for debugging purposes and to load some
-- functions into ghci
module Debug where

import Import
import Language
import TermGen
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

genSt :: GenState
genSt = mkGenState (fromModule prelude) EtaLong mempty

instance (Pretty a, Pretty b) => Pretty (Map a b) where
  pretty m = align . Prettyprinter.list $ Map.assocs m <&> \(k, x) ->
    pretty k <> ":" <+> pretty x

instance Pretty a => Pretty (Set a) where
  pretty = pretty . Set.toList

instance Pretty HoleCtx where
  pretty (HoleCtx t xs) = parens ("::" <+> pretty t) <> "," <+> pretty xs

instance Pretty Concept

tryGen :: Monad m => GenT m a -> m a
tryGen x = evalGenT x prelude genSt
