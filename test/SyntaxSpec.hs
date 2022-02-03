module SyntaxSpec where

import Import
import Test.Hspec
import Test.Hspec.QuickCheck
import Language.Syntax
import Language.Parser
import Language.Utils
import Prettyprinter.Render.Text
import Text.Megaparsec
import RIO.Set as Set

prettyThenParse :: forall a. (Pretty a, Parse a, Eq a) => a -> Bool
prettyThenParse x =
  let t = renderStrict . layoutCompact . pretty $ x
  in case parse (parser @a) "" t of
    Right y -> x == y
    Left _ -> False

punchAndDissect :: Expr Void -> Bool
punchAndDissect x =
  ((==) `on` Set.fromList)
    (dissect x)
    (concatMap holes $ punch x)

spec :: Spec
spec = do
  describe "parse is inverse of pretty" do
    prop "Type" $ prettyThenParse @(Type Hole)
    prop "Expr Hole" $ prettyThenParse @(Expr Hole)
    prop "Expr Type" $ prettyThenParse @(Expr (Type Hole))
  prop "punch-dissect" punchAndDissect

-- TODO: add unit tests to check some common synthesis examples
