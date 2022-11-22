{-# INCLUDE foldr, snoc #-}
-- {-# INCLUDE foldList #-}
-- {-# INCLUDE foldl #-}

reverse :: List a -> List a
-- reverse = _

-- NOTE: this is exhaustive, because reverse cannot be implemented using map
-- reverse = map _
-- Whereas this leads to timeout! Because xs could be used in the hole, so we
-- cannot merge the constraints [A] <== [A] and [A,B] <== [B,A]. This seems to
-- indicate that there are advantages to not always eta-expanding. However,
-- this has more to do with where exactly xs is allowed to be used, which we
-- might want to limit in certain situations.
reverse xs = map _ xs

-- assert reverse []        <== []
assert reverse [A]       <== [A]
assert reverse [A, B]    <== [B, A]
