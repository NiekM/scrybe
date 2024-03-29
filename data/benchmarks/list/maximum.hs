{-# INCLUDE map, filter, foldl, foldr, elimList #-}
{-# INCLUDE leq, elimBool #-}

{-# DESC "The largest number in a list" #-}
maximum :: List Nat -> Nat
maximum = _

assert maximum []      <== 0
assert maximum [1]     <== 1
assert maximum [1,1]   <== 1
assert maximum [0,2]   <== 2
assert maximum [2,0]   <== 2
assert maximum [3,2,1] <== 3
assert maximum [0,2,3] <== 3
