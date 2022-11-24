{-# INCLUDE foldr, elimList #-}

{-# DESC "Increment each value in a list by one" #-}
inc :: List Nat -> List Nat
inc = _

assert inc [] <== []
assert inc [1, 2] <== [2, 3]
assert inc [0, 0] <== [1, 1]
assert inc [3, 4, 5] <== [4, 5, 6]
