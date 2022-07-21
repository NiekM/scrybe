import Prelude (foldList, eq, elimBool)
-- import Prelude (foldList, eq, or)
-- import Prelude (any, eq)

elem :: Nat -> List Nat -> Bool
elem = {}

assert elem 0 [] <== False
assert elem 0 [1] <== False
assert elem 1 [1] <== True
assert elem 0 [1,0] <== True
assert elem 2 [0,2] <== True
assert elem 1 [2,0] <== False
assert elem 1 [3,2,1] <== True
assert elem 1 [0,2,3] <== False
assert elem 3 [0,2,3] <== True
assert elem 1 [0,0,0,0,0] <== False
assert elem 1 [0,0,1,0,0] <== True
