{-# INCLUDE sum, map #-}

length :: List a -> Nat
length = _

assert length [] <== 0
assert length [A] <== 1
assert length [B,A] <== 2
