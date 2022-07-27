-- import Prelude (foldList, elem, elimBool)
import Prelude (foldList, not, eq, filter)

nub :: List Nat -> List Nat
-- nub = foldList [] \x r -> elimBool (Cons x r) {} (elem x r)
nub = foldList [] \x r -> Cons x (filter (\y -> not {}) r)
-- nub = {}

assert nub [] <== []
assert nub [1] <== [1]
assert nub [1,1] <== [1]
assert nub [0,2] <== [0,2]
assert nub [2,0] <== [2,0]
assert nub [0,0,0] <== [0]
assert nub [3,2,3,2] <== [3,2]
