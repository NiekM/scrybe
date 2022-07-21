import Prelude (filter, maximum, eq, not)

dropmax :: List Nat -> List Nat
dropmax = {}

assert dropmax [] <== []
assert dropmax [1] <== []
assert dropmax [1,1] <== []
assert dropmax [0,2] <== [0]
assert dropmax [2,0] <== [0]
assert dropmax [3,2,1] <== [2,1]
assert dropmax [0,2,3] <== [0,2]
