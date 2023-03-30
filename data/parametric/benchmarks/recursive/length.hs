{-# INCLUDE foldr, elimList #-}

{-# DESC "The number of elements in a list" #-}
length :: List a -> Nat
length xs = _

assert length []     <== 0
assert length [A]    <== 1
assert length [B, C] <== 2
