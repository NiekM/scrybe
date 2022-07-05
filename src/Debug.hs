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

instance Pretty HoleCtx where
  pretty (HoleCtx t xs) = parens ("::" <+> pretty t) <> "," <+> pretty xs

instance Pretty SynState where
  pretty st = align $ vsep
    [ "Contexts:", pretty $ view contexts st
    , "", "Constraints:", pretty $ view constraints st
    , "", "Fillings:", pretty $ view fillings st
    ]

eval' :: Term Hole -> Result
eval' e = runReader (eval mempty e) prelude

uneval' :: Result -> Example -> Logic Constraints
uneval' r e = runEval prelude (uneval r $ toEx e)

assert' :: Assert -> Logic Constraints
assert' = runEval prelude . unevalAssert mempty

read :: Parse a => String -> Defs a
read s = let file = unsafePerformIO $ readFileUtf8 s in
  case lexParse parser file of
    Just x -> x
    Nothing -> error "Could not parse file"

synth' :: String -> Doc ann
synth' = pretty . fmap snd . best . runSynth prelude . synth . read
