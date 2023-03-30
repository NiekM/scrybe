{-# INCLUDE foldr #-}

-- This should be exhaustive.
tail :: List a -> List a
tail = foldr _ _

assert tail []        <== []
assert tail [A]       <== []
assert tail [A, B]    <== [B]
assert tail [A, B, C] <== [B, C]
