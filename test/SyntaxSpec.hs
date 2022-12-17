module SyntaxSpec where

import Import
import Test.Hspec
import Language.Defs
import Language.Syntax
import Language.Parser
import Language.Type
import Prettyprinter
import Prettyprinter.Render.Text
import qualified RIO.Map as Map

roundtrip :: (Pretty a, Parse a, Eq a) => a -> Bool
roundtrip x = case lexParse parser t of
  Just y -> x == y
  Nothing -> False
  where t = renderStrict . layoutPretty (LayoutOptions Unbounded) . pretty $ x

spec :: Spec
spec = do
  describe "prelude" do
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
