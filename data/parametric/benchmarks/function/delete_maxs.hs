{-# INCLUDE map, filter, foldl, foldr, elimList #-}
{-# INCLUDE maximum, neq #-}

{-# DESC "Remove the largest numbers from a list of lists" #-}
delete_maxs :: List (List Nat) -> List (List Nat)
delete_maxs = _

assert delete_maxs [] <== []
assert delete_maxs [[]] <== [[]]
assert delete_maxs [[1]] <== [[]]
assert delete_maxs [[1,1]] <== [[]]
assert delete_maxs [[1],[0]] <== [[],[0]]
assert delete_maxs [[1,0]] <== [[0]]
assert delete_maxs [[1],[2,1]] <== [[1],[1]]
assert delete_maxs [[2],[1,0]] <== [[],[1,0]]
