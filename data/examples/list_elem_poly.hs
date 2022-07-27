import Prelude (foldList, elimBool)
-- import Prelude (foldList, eq, or)
-- import Prelude (any, eq)

elem :: (a -> a -> Bool) -> Nat -> List Nat -> Bool
elem = {}

assert elem eq 0 [] <== False
assert elem eq 0 [1] <== False
assert elem eq 1 [1] <== True
assert elem eq 0 [1,0] <== True
assert elem eq 2 [0,2] <== True
assert elem eq 1 [2,0] <== False
assert elem eq 1 [3,2,1] <== True
assert elem eq 1 [0,2,3] <== False
assert elem eq 3 [0,2,3] <== True
assert elem eq 1 [0,0,0,0,0] <== False
assert elem eq 1 [0,0,1,0,0] <== True
