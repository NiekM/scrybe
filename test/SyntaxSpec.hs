module SyntaxSpec where

import Import
import Test.Hspec
import Language.Syntax
import Language.Parser
import Language.Type
import Prettyprinter
import Prettyprinter.Render.Text
import qualified RIO.Map as Map
import qualified RIO.Text as Text

prettyThenParse :: (Pretty a, Parse a, Eq a) => a -> Bool
prettyThenParse x =
  let t = renderStrict . layoutPretty defaultLayoutOptions . pretty $ x
  in case lexParse parser t of
    Just y -> x == y
    Nothing -> False

spec :: Spec
spec = do
  describe "prelude" do
    f <- runIO $ readFileUtf8 "data/prelude.hs"
    case lexParse parser f of
      Nothing -> it "parses" False
      Just x -> do
        let y = Map.assocs $ first (over holes absurd) <$> functions x
        forM_ y \(MkVar v, (e, t)) -> describe (Text.unpack v) do
          it "type checks" . isJust $ evalTCT (check e t) x emptyTC
          it "type pretty parses" $ prettyThenParse t
          it "body pretty parses" $ prettyThenParse e

-- TODO: add unit tests to check some common synthesis examples
