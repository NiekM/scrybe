-- {-# INCLUDE foldList, snoc #-}
{-# INCLUDE foldList #-}

reverse :: List a -> List a
reverse = _

assert reverse []        <== []
assert reverse [A]       <== [A]
assert reverse [A, B]    <== [B, A]
assert reverse [A, B, C] <== [C, B, A]
