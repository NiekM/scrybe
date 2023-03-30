{-# INCLUDE snoc, map, foldr #-}

-- Whereas this leads to timeout! Because xs could be used in the hole, so we
-- cannot merge the constraints [A] <== [A] and [B,C] <== [C,B]. This seems to
-- indicate that there are advantages to not always eta-expanding. However,
-- this has more to do with where exactly xs is allowed to be used, which we
-- might want to limit in certain situations.
reverse :: List a -> List a
reverse xs = _

assert reverse []        <== []
assert reverse [A]       <== [A]
assert reverse [B, C]    <== [C, B]
