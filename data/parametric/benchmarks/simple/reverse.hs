-- {-# INCLUDE foldr, snoc, map #-}
{-# INCLUDE snoc, map, foldr #-}

reverse :: List a -> List a
reverse = _

assert reverse []        <== []
assert reverse [A]       <== [A]
assert reverse [B, C]    <== [C, B]
