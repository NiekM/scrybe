{-# INCLUDE foldList, plus #-}
-- {-# INCLUDE foldList, foldNat #-}

sum :: List Nat -> Nat
sum = _

assert sum []     <== 0
assert sum [1]    <== 1
assert sum [2, 1] <== 3
assert sum [1, 3] <== 4
