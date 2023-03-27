module TypeSpec where

import Import
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Language.Defs
import Language.Syntax
import Language.Parser
import Language.Type
import qualified RIO.Map as Map

instance Arbitrary Mono where
  arbitrary = do
    n <- chooseInt (1, 3)
    arrs <$> vectorOf n arg
    where
      arg = do
        n <- frequency [(5, return 0), (2, return 1), (1, return 2)]
        xs <- vectorOf n arg
        oneof
          [ do
            c <- MkCtr . (<> fromString (show n)) <$> elements ["A", "B", "C"]
            pure $ Ctr c xs
          , Var . MkFree <$> elements ["a", "b", "c"]
          ]

spec :: Spec
spec = do
  describe "unify" do
    -- NOTE: the danger here is that this one only ever succeeds when one of
    -- the arguments is just a free variable. It might be better to
    -- specifically generate types for which unification makes sense.
    prop "unifies" \t u -> case unify t u of
      Nothing -> discard
      Just th -> subst th t == subst th u
    prop "reflexive" \t -> isJust (unify t t)
    -- NOTE: This does not check that the resulting unifications are
    -- equivalent, which is quite tricky. This just checks symmetry if we look
    -- at the result as boolean.
    prop "symmetric" \t u -> case unify t u of
      Nothing -> discard
      Just _ -> isJust (unify u t)
