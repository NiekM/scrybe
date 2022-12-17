{-# INCLUDE elimList #-}

head :: List a -> a
head = _

assert head [A]       <== A
assert head [A, B]    <== A
assert head [A, B, C] <== A
