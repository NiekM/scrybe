-- {-# INCLUDE foldNat, elimBool #-}

{-# INCLUDE foldrNat, elimBool #-}

{-# DESC "Whether a natural number is even." #-}
even :: Nat -> Bool
even = _

assert even 0 <== True
assert even 1 <== False
assert even 2 <== True
assert even 3 <== False
