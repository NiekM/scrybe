import Prelude (compareNat, elimOrd)

max :: Nat -> Nat -> Nat
max = {}

assert max 0 0 <== 0
assert max 0 1 <== 1
assert max 0 2 <== 2
assert max 1 0 <== 1
assert max 1 1 <== 1
assert max 1 2 <== 2
assert max 2 0 <== 2
assert max 2 1 <== 2
assert max 2 2 <== 2
