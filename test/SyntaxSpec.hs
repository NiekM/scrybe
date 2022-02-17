module SyntaxSpec where

import Import
import Test.Hspec
import Test.Hspec.QuickCheck
import Language.Syntax
import Language.Parser
import Prettyprinter
import Prettyprinter.Render.Text
import Text.Megaparsec

prettyThenParse :: (Pretty a, Parse a, Eq a) => a -> Bool
prettyThenParse x =
  let t = renderStrict . layoutCompact . pretty $ x
  in case parse parser "" t of
    Right y -> x == y
    Left _ -> False

spec :: Spec
spec = do
  describe "parse is inverse of pretty" do
    prop "Type Free" $ prettyThenParse @(Type Free)
    prop "Term Hole" $ prettyThenParse @(Term Hole)
    prop "Term (Type Free)" $ prettyThenParse @(Term (Type Free))

-- TODO: add unit tests to check some common synthesis examples
