{-# INCLUDE zipWith, foldrNat #-}
-- {-# INCLUDE foldrNat #-}
-- {-# INCLUDE zipWith, plus #-}

zipPlus :: List Nat -> List Nat -> List Nat
zipPlus = _

-- assert zipWith plus [0,0,1,1,2,2] [0,1,1,2,0,2] <== [0,1,2,3,2,4]
assert zipPlus [1,0,2,3] [0,2,1,1] <== [1,2,3,4]
-- assert zipPlus [0,1] [2,3] <== [2,4]
-- assert zipPlus [0,0,1,1,2,2] [0,1,1,2,0,2] <== [0,1,2,3,2,4]
