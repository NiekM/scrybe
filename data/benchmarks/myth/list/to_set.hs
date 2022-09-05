{-# INCLUDE foldList, set_insert #-}

to_set :: List Nat -> List Nat
to_set = _

assert to_set []      <== []
assert to_set [1]     <== [1]
assert to_set [2,1]   <== [1,2]
assert to_set [0,0]   <== [0]
assert to_set [1,0,0] <== [0,1]
assert to_set [2,3,1] <== [1,2,3]
