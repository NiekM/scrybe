{-# INCLUDE foldr, plus #-}
-- {-# INCLUDE foldl, plus #-}
-- {-# INCLUDE foldr, foldNat #-}

{-# DESC "The sum of all numbers in a list" #-}
sum :: List Nat -> Nat
sum = _

assert sum []     <== 0
assert sum [1]    <== 1
assert sum [2, 1] <== 3
assert sum [1, 3] <== 4
