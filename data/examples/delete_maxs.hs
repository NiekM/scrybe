import Prelude (map, filter, eq, maximum, not)

-- TODO: how do we define minimum for Nats?
-- NOTE: this takes too long
delete_maxs :: List (List Nat) -> List (List Nat)
delete_maxs = _

assert delete_maxs [] <== []
assert delete_maxs [[]] <== [[]]
assert delete_maxs [[1, 2]] <== [[1]]
assert delete_maxs [[0], [0]] <== [[], []]
assert delete_maxs [[3, 4], [5]] <== [[3,4], []]
assert delete_maxs [[3, 4], [2, 4]] <== [[3], [2]]
assert delete_maxs [[2,1,0]] <== [[1,0]]
assert delete_maxs [[0,1,2]] <== [[0,1]]
