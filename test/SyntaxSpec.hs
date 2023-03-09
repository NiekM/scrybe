module SyntaxSpec where

import Import
import Test.Hspec hiding (Example)
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (Result)
import Language.Defs
import Language.Syntax
import Language.Parser
import Language.Type
import Prettyprinter
import Prettyprinter.Render.Text
import qualified RIO.Map as Map

instance Arbitrary Value where
  arbitrary = do
    n <- frequency [(5, return 0), (2, return 1), (1, return 2)]
    apps
      <$> (Ctr . MkCtr . (<> fromString (show n)) <$> elements ["A", "B", "C"])
      <*> vector n

instance Arbitrary Example where
  arbitrary = do
    n <- chooseInt (0, 3)
    lams <$> vector n <*> output
    where
      output = do
        n <- frequency [(5, return 0), (2, return 1), (1, return 2)]
        apps
          <$> (Ctr . MkCtr . (<> fromString (show n)) <$>
            elements ["A", "B", "C"])
          <*> vectorOf n (oneof [output, return $ Hole Unit])

roundtrip :: (Pretty a, Parse a, Eq a) => a -> Bool
roundtrip x = case lexParse parser t of
  Just y -> x == y
  Nothing -> False
  where t = renderStrict . layoutPretty (LayoutOptions Unbounded) . pretty $ x

prelude :: Spec
prelude = do
  f <- runIO $ readFileUtf8 "data/prelude.hs"
  case lexParse parser f of
    Nothing -> it "parses" False
    Just x -> do
      it "roundtrips" $ roundtrip x
      let es = Map.fromList $ bindings x <&>
            \(MkBinding a e) -> (a, over holes (absurd @Unit) e)
      let ts = Map.fromList $ signatures x <&> \(MkSignature a t) -> (a, t)
      let y = Map.assocs $ Map.intersectionWith (,) es ts
      let m = fromDefs x
      forM_ y \(v, (e, t)) -> describe (show (pretty v)) do
        it "type checks" . isJust $ evalInfer (check mempty e t) 0 m

value :: Spec
value = do
  describe "roundtrips via" do
    prop "term" \v -> downcast (upcast @('Term Void) v) == Just v
    prop "sketch" \v -> downcast (upcast @('Term Unit) v) == Just v
    prop "result" \v -> downcast (upcast @'Det v) == Just v
    prop "example" \v -> downcast (upcast @'Example v) == Just v

ex :: Spec
ex = prop "roundtrips via ex" \x -> fromEx (toEx x) == [x]

spec :: Spec
spec = do
  describe "prelude" prelude
  describe "value" value
  describe "example" ex
