module SyntaxSpec where

import Import
import Test.Hspec
import Test.Hspec.QuickCheck
import Language.Syntax
import Language.Parser
import Prettyprinter.Render.Text
import Text.Megaparsec

prettyThenParse :: forall a. (Pretty a, Parse a, Eq a) => a -> Bool
prettyThenParse x =
  let t = renderStrict . layoutCompact . pretty $ x
  in case parse (parser @a) "" t of
    Right y -> x == y
    Left _ -> False

spec :: Spec
spec = describe "parse is inverse of pretty" do
  prop "Type" $ prettyThenParse @Type
  prop "Expr Hole" $ prettyThenParse @(Expr Hole)
  prop "Expr Type" $ prettyThenParse @(Expr Type)
