{-# INCLUDE leq #-}
{-# INCLUDE elimBool #-}

{-# DESC "The largest of two natural numbers." #-}
max :: Nat -> Nat -> Nat
max = _

assert max 0 0 <== 0
assert max 0 1 <== 1
assert max 0 2 <== 2
assert max 1 0 <== 1
assert max 1 1 <== 1
assert max 1 2 <== 2
assert max 2 0 <== 2
assert max 2 1 <== 2
assert max 2 2 <== 2
