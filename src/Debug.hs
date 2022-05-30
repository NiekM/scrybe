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
import qualified RIO.List as List
import qualified RIO.Map as Map
import qualified RIO.Set as Set

fromStr :: Parse a => String -> a
fromStr = fromMaybe (error "Parse failed") . lexParse parser . T.pack

newtype Parseable a = Parseable a

instance Parse a => IsString (Parseable a) where
  fromString = Parseable . fromStr

deriving via Parseable (Expr l) instance Parse (Expr l) => IsString (Expr l)
deriving via Parseable Poly     instance IsString Poly
deriving via Parseable Assert   instance IsString Assert

prelude :: Mod
prelude = let file = unsafePerformIO $ readFileUtf8 "data/prelude.hs" in
  case lexParse parser file of
    Just x -> x
    Nothing -> error "Could not parse prelude"

instance (Pretty a, Pretty b) => Pretty (Map a b) where
  pretty m = align . Prettyprinter.list $ Map.assocs m <&> \(k, x) ->
    pretty k <> ":" <+> align (pretty x)

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

trySyn :: Monad m => RWST Mod () SynState m a -> m (a, SynState)
trySyn x = runSyn x prelude

tryTC :: Monad m => RWST Mod () FreshState m a -> m a
tryTC x = fst <$> runTC x prelude

tryUneval :: Monad m => RWST Mod () () m a -> m a
tryUneval m = fst <$> evalRWST m prelude ()

eval' :: Term Hole -> Result
eval' e = runReader (eval mempty e) prelude

uneval' :: Result -> Example -> [Uneval]
uneval' r e = tryTC (uneval r e)

assert' :: Assert -> [Uneval]
assert' = tryTC . unevalAssert mempty

read :: Parse a => String -> Defs a
read s = let file = unsafePerformIO $ readFileUtf8 s in
  case lexParse parser file of
    Just x -> x
    Nothing -> error "Could not parse file"

synth' :: Defs Unit -> [Map Hole (Term Hole)]
synth' = synth prelude

test :: Term Hole -> Example -> [(Hole, Term Hole)] -> Doc ann
test sk ex rs = pretty . tryUneval @[] $
  foldl' (\r (h, e) -> r >>= resumeUneval h e) (uneval (eval' sk) ex) rs

{- NOTE: interesting example, try out different (orders of) hole fillings

>>> xs = uneval (eval' "map (\\x -> {0}) {1}") "[1,2]"
>>> pretty . tryUneval @[] $ xs
  >>= resumeUneval 0 "Succ {2}"
  >>= resumeUneval 1 "[0, 1]"
  >>= resumeUneval 2 "x"

-}
