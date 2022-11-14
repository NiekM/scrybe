{-# INCLUDE foldr, elimMaybe #-}

last :: List a -> Maybe a
last = _

assert last []        <== Nothing
assert last [A]       <== Just A
assert last [A, B]    <== Just B
assert last [A, B, C] <== Just C
