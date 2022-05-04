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

prelude :: Module
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

tryTC :: Monad m => RWST Module () FreshState m a -> m a
tryTC x = fst <$> runTC x prelude

imports :: Set Var
imports = Set.fromList ["succ", "zero", "nil", "cons"]

-- TODO: replace this with an 'import' function that selects what is imported
-- from the prelude. Perhaps even implement this into model solutions so that
-- you can write `import Prelude (...); model = ...`
prelude' :: Module
prelude' = prelude { funs = Map.restrictKeys (funs prelude) imports }

trySyn' :: Monad m => RWST Module () SynState m a -> m a
trySyn' x = fst <$> runSyn x prelude' (mkSynState (fromModule prelude') mempty)

trySyn :: Monad m => RWST Module () SynState m a -> m a
trySyn x = fst <$> runSyn x prelude synSt

eval' :: Term Hole -> Result
eval' = eval $ liveEnv prelude

uneval' :: Result -> Example -> [UC]
uneval' r e = tryTC (uneval r e)
