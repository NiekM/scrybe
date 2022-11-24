{-# INCLUDE map, filter, foldl, foldr, elimList #-}
{-# INCLUDE maximum, neq #-}

{-# DESC "Remove the largest numbers from a list" #-}
delete_max :: List Nat -> List Nat
delete_max = _

assert delete_max [] <== []
assert delete_max [1] <== []
assert delete_max [1,1] <== []
assert delete_max [0,2] <== [0]
assert delete_max [2,0] <== [0]
assert delete_max [3,2,1] <== [2,1]
assert delete_max [0,2,3] <== [0,2]
