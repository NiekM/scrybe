{-# INCLUDE reverse, eqList #-}

palindrome :: List Nat -> List Nat
palindrome x = eqList x x

assert palindrome [] <== True
assert palindrome [0] <== True
assert palindrome [1] <== True
assert palindrome [0,1] <== False
assert palindrome [0,1,0] <== True
assert palindrome [1,0] <== False
assert palindrome [0,0] <== True
assert palindrome [0,0,0] <== True
assert palindrome [1,0,0] <== False
assert palindrome [0,1,1,0] <== True
