module SyntaxSpec where

import Import
import Test.Hspec
import Language.Syntax
import Language.Parser
import Language.Type
import Prettyprinter
import Prettyprinter.Render.Text
import qualified RIO.Text as Text
import qualified RIO.Map as Map

roundtrip :: (Pretty a, Parse a, Eq a) => a -> Bool
roundtrip x =
  let t = renderStrict . layoutPretty defaultLayoutOptions . pretty $ x
      u = Text.intercalate " " . Text.lines $ t
  in case lexParse parser u of
    Just y -> x == y
    Nothing -> False

spec :: Spec
spec = do
  describe "prelude" do
    f <- runIO $ readFileUtf8 "data/prelude.hs"
    case lexParse parser f of
      Nothing -> it "parses" False
      Just x -> do
        let es = Map.fromList $ bindings x <&>
              \(MkBinding a e) -> (a, over holes (absurd @Unit) e)
        let ts = Map.fromList $ signatures x <&> \(MkSignature a t) -> (a, t)
        let y = Map.assocs $ Map.intersectionWith (,) es ts
        let m = fromDefs x
        forM_ y \(v, (e, t)) -> describe (show (pretty v)) do
          it "type checks" . isJust $ evalInfer (check mempty e t) 0 m
          it "type roundtrips" $ roundtrip t
          it "body roundtrips" $ roundtrip e
