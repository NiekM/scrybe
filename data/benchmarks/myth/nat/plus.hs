import Prelude (foldNat)

plus :: Nat -> Nat -> Nat
plus = {}

assert plus 0 0 <== 0
assert plus 0 1 <== 1
assert plus 1 1 <== 2
assert plus 1 2 <== 3
assert plus 2 0 <== 2
assert plus 2 2 <== 4

-- NOTE: removed some assertions
-- assert plus 0 2 <== 2
-- assert plus 1 0 <== 1
-- assert plus 2 1 <== 3
