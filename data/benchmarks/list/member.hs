{-# INCLUDE foldr, eq, elimBool #-}
-- {-# INCLUDE any, eq #-}

{-# DESC "Whether a number occurs in a list" #-}
member :: Nat -> List Nat -> Bool
member = _

assert member 0 []          <== False
assert member 0 [1]         <== False
assert member 1 [1]         <== True
assert member 0 [1,0]       <== True
assert member 2 [0,2]       <== True
assert member 1 [2,0]       <== False
assert member 1 [3,2,1]     <== True
assert member 1 [0,2,3]     <== False
assert member 3 [0,2,3]     <== True
assert member 1 [0,0,0,0,0] <== False
assert member 1 [0,0,1,0,0] <== True
