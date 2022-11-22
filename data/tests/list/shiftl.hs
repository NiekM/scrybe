{-# INCLUDE foldr, elimList #-}

{-# DESC "Shift all elements in a list to the left" #-}
shiftl :: List a -> List a
shiftl = _

assert shiftl [] <== []
assert shiftl [A] <== [A]
assert shiftl [A,B] <== [B,A]
assert shiftl [A,B,C] <== [B,C,A]
