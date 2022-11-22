{-# INCLUDE foldr, filter, neq #-}

{-# DESC "Remove duplicates from a list" #-}
nub :: List Nat -> List Nat
nub = _

assert nub []      <== []
assert nub [1]     <== [1]
assert nub [2,1]   <== [2,1]
assert nub [0,0]   <== [0]
assert nub [1,0,0] <== [1,0]
assert nub [2,3,1] <== [2,3,1]
assert nub [1,1,1] <== [1]
