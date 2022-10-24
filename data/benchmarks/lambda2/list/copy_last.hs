{-# INCLUDE foldr, elimList #-}

copy_last :: List a -> List a
copy_last = _

assert copy_last [] <== []
assert copy_last [A] <== [A]
assert copy_last [A,B] <== [B,B]
assert copy_last [A,B,C] <== [C,C,C]
