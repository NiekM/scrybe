{-# INCLUDE map, filter, foldl, foldr, elimList #-}

{-# DESC "Replace each element in a list with the last" #-}
copy_last :: List a -> List a
copy_last = _

assert copy_last [] <== []
assert copy_last [A] <== [A]
assert copy_last [A,B] <== [B,B]
assert copy_last [A,B,C] <== [C,C,C]
