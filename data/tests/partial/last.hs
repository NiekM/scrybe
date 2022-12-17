{-# INCLUDE paraList, elimList #-}

last :: List a -> a
last = _

assert last [A]       <== A
assert last [A, B]    <== B
assert last [A, B, C] <== C
