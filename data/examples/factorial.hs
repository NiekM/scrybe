{-# INCLUDE paraNat, mult #-}

factorial :: Nat -> Nat
factorial = _

assert factorial 0 <== 1
assert factorial 1 <== 1
assert factorial 2 <== 2
assert factorial 3 <== 6
-- assert factorial 4 <== 24
