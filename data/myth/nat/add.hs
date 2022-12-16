{-# INCLUDE foldrNat #-}

{-# DESC "Add two natural numbers." #-}
add :: Nat -> Nat -> Nat
add = _

assert add 0 0 <== 0
assert add 0 1 <== 1
assert add 1 1 <== 2
assert add 1 2 <== 3
assert add 2 0 <== 2
assert add 2 2 <== 4
