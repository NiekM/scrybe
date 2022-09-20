{-# INCLUDE foldrNat #-}

replicate :: Nat -> a -> List a
replicate = _

assert replicate 0 A <== []
assert replicate 1 B <== [B]
assert replicate 2 C <== [C, C]
assert replicate 3 D <== [D, D, D]
