{-# INCLUDE paraList, elimList #-}

init :: List a -> List a
init = _

assert init [A]     <== []
assert init [A,B]   <== [A]
assert init [A,B,C] <== [A,B]
