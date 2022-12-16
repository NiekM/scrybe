{-# INCLUDE foldrNat :: (Nat -> b) -> ((Nat -> b) -> Nat -> b) -> Nat -> Nat -> b #-}
{-# INCLUDE elimNat #-}

compare :: Nat -> Nat -> Ord
compare = _

assert compare 0 0 <== EQ
assert compare 0 1 <== LT
assert compare 1 0 <== GT
assert compare 1 1 <== EQ
assert compare 1 3 <== LT
assert compare 3 2 <== GT
