import Prelude (zipWith, foldNat)
-- import Prelude (zipWith, plus)

-- TODO: allow explicit importing of functions with specific types, like this:
--
-- import zipWith :: (Nat -> Nat -> Nat) -> List Nat -> List Nat -> List Nat
-- import foldNat :: (Nat -> Nat) -> Nat -> Nat -> Nat
--
-- and/or like this:
--
-- import zipWith @Nat @Nat @Nat
-- import foldNat @Nat
--
-- maybe use foreign keyword i.o. import?

zipPlus :: List Nat -> List Nat -> List Nat
zipPlus = {}

-- assert zipWith plus [0,0,1,1,2,2] [0,1,1,2,0,2] <== [0,1,2,3,2,4]
assert zipPlus [] [] <== []
assert zipPlus [0,1] [2,3] <== [2,4]
assert zipPlus [0,0,1,1,2,2] [0,1,1,2,0,2] <== [0,1,2,3,2,4]
