import Prelude (foldNatIndexed, elimNat)
-- import Prelude (elimNat)

-- NOTE: adding foldNat to the environment makes the computation run out fuel
-- very quickly.
compare :: Nat -> Nat -> Ord
-- compare x y = foldNat (\z -> elimNat {} {} {}) {} y x
-- TODO: add foldNatIndexed or something
-- compare x y = foldNatIndexed {} {} y x
compare = {}

assert compare 0 0 <== EQ
assert compare 0 1 <== LT
assert compare 1 0 <== GT
assert compare 1 1 <== EQ
assert compare 1 3 <== LT
assert compare 3 2 <== GT
