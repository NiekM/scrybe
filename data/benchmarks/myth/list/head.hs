{-# INCLUDE elimList #-}

head :: List a -> Maybe a
head = _

assert head []        <== Nothing
assert head [A]       <== Just A
assert head [A, B]    <== Just A
assert head [A, B, C] <== Just A
