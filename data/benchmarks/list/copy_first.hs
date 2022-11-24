{-# INCLUDE map, filter, foldl, foldr, elimList #-}

{-# DESC "Replace each element in a list with the first" #-}
copy_first :: List a -> List a
copy_first = _

assert copy_first [] <== []
assert copy_first [A] <== [A]
assert copy_first [A,B] <== [A,A]
assert copy_first [A,B,C] <== [A,A,A]
