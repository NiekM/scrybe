{-# INCLUDE foldr, elimList #-}

intersperse :: a -> List a -> List a
intersperse = _

assert intersperse A [] <== []
assert intersperse A [B] <== [B]
assert intersperse A [B,C] <== [B,A,C]
assert intersperse A [B,C,D] <== [B,A,C,A,D]
