module SyntaxSpec where

import Import
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Language.Syntax
import Language.Parser
import Language.Utils
import Prettyprinter.Render.Text
import Text.Megaparsec
import RIO.Set as Set

prettyThenParse :: (Pretty a, Parse a, Eq a) => a -> Bool
prettyThenParse x =
  let t = renderStrict . layoutCompact . pretty $ x
  in case parse parser "" t of
    Right y -> x == y
    Left _ -> False

punchAndDissect :: Term Void -> Bool
punchAndDissect x =
  ((==) `on` Set.fromList)
    (dissect x)
    (concatMap holes $ punch x)

spec :: Spec
spec = do
  describe "parse is inverse of pretty" do
    prop "Type Hole" $ prettyThenParse @(Type Hole)
    prop "Term Hole" $ prettyThenParse @(Term Hole)
    prop "Term (Type Hole)" $ prettyThenParse @(Term (Type Hole))
  prop "punch-dissect" (forAll (scale (`div` 3) arbitrary) punchAndDissect)

-- TODO: add unit tests to check some common synthesis examples
