
-- NOTE: depending on whether the first or last occurence of elements should
-- be preserved, we have different definitions. As such, for the variant using
-- `filter` we need examples that preserve first occurence and for the variant
-- using `elem` we need examples that preserve last occurence.

-- {-# INCLUDE foldr, not, eq, filter #-}

{-# INCLUDE foldr, elem, elimBool #-}

nub :: List Nat -> List Nat
nub = _

assert nub [] <== []
assert nub [1] <== [1]
assert nub [1,1] <== [1]
assert nub [0,2] <== [0,2]
assert nub [2,0] <== [2,0]
assert nub [0,0,0] <== [0]
assert nub [3,2,3,2] <== [3,2]

-- First occurence:
-- assert nub [3,2,3,0] <== [3,2,0]

-- Last occurence:
assert nub [3,2,3,0] <== [2,3,0]
