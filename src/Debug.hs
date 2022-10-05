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
import Prettyprinter hiding (fill)
import Control.Monad.State
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

tryUneval :: Uneval a -> Maybe a
tryUneval x = view _1 <$> runRWST x (view scope prelude, ctrArity prelude) 1000

eval' :: Term Hole -> Result
eval' e = runReader (eval mempty e) (view scope prelude)

uneval' :: Result -> Example -> Maybe (Logic Constraints)
uneval' r e = tryUneval (uneval r $ toEx e)

assert' :: Assert -> Maybe (Logic Constraints)
assert' = tryUneval . unevalAssert mempty

normConstraints :: Logic Constraints -> Logic Constraints
normConstraints cs = Disjunction $ dnf cs & mapMaybe
  \xs -> case nubOrd (mergeConstraints xs) of
    [] -> Nothing
    ys -> Just . Conjunction . fmap Pure $ ys

merge :: Maybe (Logic Constraints) -> Doc ann
merge = pretty . fmap (fmap mergeConstraints . dnf)

read :: Parse a => String -> Defs a
read s = let file = unsafePerformIO $ readFileUtf8 s in
  case lexParse parser file of
    Just x -> x
    Nothing -> error "Could not parse file"

synth' :: String -> Doc ann
synth' s = case run defs of
  Nothing -> "Failed"
  Just (n, hf) -> vsep
    [ "depth:" <+> pretty (fromIntegral n :: Int)
    , pretty hf
    , pretty $ relBinds defs <&> \(MkBinding x e) ->
      MkBinding x (fill (normalizeFilling hf) e)
    ]
  where
    defs = read s
    run = best . runNondet . runSynth prelude . synth

synthN :: Int -> String -> Doc ann
synthN n s = vsep $ run defs <&> \hf -> vsep
    [ pretty hf
    , pretty $ relBinds defs <&> \(MkBinding x e) ->
      MkBinding x (fill (normalizeFilling hf) e)
    ]
  where
    defs = read s
    run = fmap fst . take n . search . runNondet . runSynth prelude . synth

relBinds :: Defs Unit -> [Binding Hole]
relBinds (Defs ds) = bs' <&> uncurry MkBinding
  where
    bs = [ (x, e) | Binding (MkBinding x e) <- ds, isNothing (holeFree e)]
    bs' = evalState (forOf (each . _2 . holes) bs $ const fresh) mkFreshState
