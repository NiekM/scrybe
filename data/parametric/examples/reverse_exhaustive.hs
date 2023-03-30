{-# INCLUDE snoc, map, foldr #-}

-- NOTE: this is exhaustive, because reverse cannot be implemented using map
reverse :: List a -> List a
reverse = map _

assert reverse []        <== []
assert reverse [A]       <== [A]
assert reverse [B, C]    <== [C, B]
