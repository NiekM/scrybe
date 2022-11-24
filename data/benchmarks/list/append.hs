{-# INCLUDE map, filter, foldl, foldr, elimList #-}

{-# DESC "Append two lists" #-}
append :: List a -> List a -> List a
append = _

assert append []     []  <== []
assert append []     [A] <== [A]
assert append [A]    []  <== [A]
assert append [A]    [B] <== [A, B]
assert append [A, B] []  <== [A, B]
assert append [A, B] [C] <== [A, B, C]
