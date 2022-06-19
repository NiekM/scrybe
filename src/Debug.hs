{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- NOTE: this module is purely for debugging purposes and to load some
-- functions and instances into ghci
module Debug where

import Import
import Language
import Nondet
import Synthesis
import qualified RIO.Text as T
import System.IO.Unsafe
import Prettyprinter
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import Control.Monad.Heap

fromStr :: Parse a => String -> a
fromStr = fromMaybe (error "Parse failed") . lexParse parser . T.pack

newtype Parseable a = Parseable a

instance Parse a => IsString (Parseable a) where
  fromString = Parseable . fromStr

deriving via Parseable (Expr l) instance Parse (Expr l) => IsString (Expr l)
deriving via Parseable Poly     instance IsString Poly
deriving via Parseable Assert   instance IsString Assert

prelude :: Env
prelude = let file = unsafePerformIO $ readFileUtf8 "data/prelude.hs" in
  case lexParse parser file of
    Just x -> x
    Nothing -> error "Could not parse prelude"

instance (Pretty a, Pretty b) => Pretty (Map a b) where
  pretty m = align . Prettyprinter.list $ Map.assocs m <&> \(k, x) ->
    pretty k <> ":" <+> align (pretty x)

instance (Pretty k, Pretty v) => Pretty (Tree k v) where
  pretty = \case
    Node a -> pretty a
    Branch xs -> pretty xs

instance Pretty a => Pretty (Set a) where
  pretty = pretty . Set.toList

instance Pretty HoleCtx where
  pretty (HoleCtx t xs) = parens ("::" <+> pretty t) <> "," <+> pretty xs

instance Pretty Ex where
  pretty = pretty . fromEx

instance Pretty SynState where
  pretty st = align $ vsep
    [ "Contexts:", pretty $ view contexts st
    , "", "Constraints:", pretty $ view constraints st
    , "", "Fillings:", pretty $ view fillings st
    ]

tryUneval :: Uneval a -> Nondet a
tryUneval x = view _1 <$> runRWST x prelude 1000

eval' :: Term Hole -> Result
eval' e = runReader (eval mempty e) prelude

uneval' :: Result -> Example -> Nondet Constraints
uneval' r e = tryUneval (uneval r e)

assert' :: Assert -> Nondet Constraints
assert' = tryUneval . unevalAssert mempty

read :: Parse a => String -> Defs a
read s = let file = unsafePerformIO $ readFileUtf8 s in
  case lexParse parser file of
    Just x -> x
    Nothing -> error "Could not parse file"

synth' :: String -> Doc ann
synth' = pretty . fmap snd . best . runSynth prelude . synth . read
