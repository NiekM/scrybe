{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- NOTE: this module is purely for debugging purposes and to load some
-- functions and instances into ghci
module Debug where

import Import
import Language
import Synthesis
import qualified RIO.Text as T
import System.IO.Unsafe
import Prettyprinter
import qualified RIO.Map as Map
import qualified RIO.Set as Set

fromStr :: Parse a => String -> a
fromStr = fromMaybe (error "Parse failed") . lexParse parser . T.pack

instance Parse (Expr l) => IsString (Expr l) where fromString = fromStr
instance IsString Poly where fromString = fromStr

prelude :: Mod
prelude = let file = unsafePerformIO $ readFileUtf8 "data/prelude.hs" in
  case lexParse parser file of
    Just x -> x
    Nothing -> error "Could not parse prelude"

synSt :: SynState
synSt = mkSynState (fromModule prelude) mempty

instance (Pretty a, Pretty b) => Pretty (Map a b) where
  pretty m = align . Prettyprinter.list $ Map.assocs m <&> \(k, x) ->
    pretty k <> ":" <+> align (pretty x)

instance Pretty a => Pretty (Set a) where
  pretty = pretty . Set.toList

instance Pretty HoleCtx where
  pretty (HoleCtx t xs) = parens ("::" <+> pretty t) <> "," <+> pretty xs

instance Pretty Ex where
  pretty = pretty . fromEx

tryTC :: Monad m => RWST Mod () FreshState m a -> m a
tryTC x = fst <$> runTC x prelude

trySyn' :: Monad m => Mod -> RWST Mod () SynState m a -> m a
trySyn' m x = evalSyn x m (mkSynState (fromModule m) mempty)

trySyn :: Monad m => RWST Mod () SynState m a -> m a
trySyn x = evalSyn x prelude synSt

eval' :: Term Hole -> Result
eval' e = runReader (eval mempty e) prelude

uneval' :: Result -> Example -> [Uneval]
uneval' r e = tryTC (uneval r e)

{- NOTE: interesting example, try out different (orders of) hole fillings

>>> xs = uneval (eval' "map (\\x -> {0}) {1}") "[1,2]"
>>> pretty . trySyn @[] $ xs
  >>= resumeUneval 0 "Succ {2}"
  >>= resumeUneval 1 "[0, 1]"
  >>= resumeUneval 2 "x"

-}
