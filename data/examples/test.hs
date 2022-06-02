-- import Prelude (foldNat)
import Prelude

-- double :: Nat -> Nat
-- -- double = {}
-- double x = foldNat {} {} {}

-- -- assert map double [0,1,2] <== [0,2,4]
-- -- assert map double [0,1,2,3,4,5] <== [0,2,4,6,8,10]

-- assert double 0 <== 0
-- assert double 1 <== 2
-- assert double 2 <== 4
-- -- assert double 3 <== 6
-- -- assert double 4 <== 8
-- -- assert double 5 <== 10
-- -- assert double 6 <== 12
-- -- assert double 7 <== 14
-- -- assert double 8 <== 16
-- -- assert double 9 <== 18

plus1 :: Nat -> Nat
plus1 n = elimNat {} {} {}

assert plus1 0 <== 1
assert plus1 1 <== 2
assert plus1 2 <== 3
assert plus1 3 <== 4
assert plus1 4 <== 5
assert plus1 5 <== 6
assert plus1 6 <== 7
assert plus1 7 <== 8
assert plus1 8 <== 9
assert plus1 9 <== 10


-- import Prelude (elimBool)

-- -- not :: Bool -> Bool
-- -- not x = elimBool {} {} {}

-- -- assert not True <== False
-- -- assert not True <== False
-- -- assert not False <== True

-- xor :: Bool -> Bool -> Bool
-- xor x y = elimBool {} {} x

-- -- TODO: why are these conflicting?
-- assert xor True  True  <== False
-- assert xor True  True  <== False
-- assert xor True  False <== True
-- assert xor False True  <== True
-- assert xor False False <== False
