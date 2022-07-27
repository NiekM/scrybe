import Prelude (filter, maximum, eq, not)

delete_max :: List Nat -> List Nat
delete_max = _

assert delete_max [] <== []
assert delete_max [1] <== []
assert delete_max [1,1] <== []
assert delete_max [0,2] <== [0]
assert delete_max [2,0] <== [0]
assert delete_max [3,2,1] <== [2,1]
assert delete_max [0,2,3] <== [0,2]
