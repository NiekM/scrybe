{-# INCLUDE foldr, elimBool #-}
-- {-# INCLUDE any #-}

elem :: (a -> a -> Bool) -> a -> List a -> Bool
elem = _

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
