{-# INCLUDE zipWith, foldrNat #-}

zipPlus :: List Nat -> List Nat -> List Nat
zipPlus = _

assert zipPlus [1,0,2,3] [0,2,1,1] <== [1,2,3,4]
