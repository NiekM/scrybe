{-# INCLUDE map, filter, foldl, foldr, elimList #-}
{-# INCLUDE reverse #-}

{-# DESC "Shift all elements in a list to the left" #-}
shiftl :: List a -> List a
shiftl = _

assert shiftl []          <== []
assert shiftl [A]         <== [A]
assert shiftl [A,B]       <== [B,A]
assert shiftl [A,B,C]     <== [B,C,A]
assert shiftl [A,B,C,D]   <== [B,C,D,A]
assert shiftl [A,B,C,D,E] <== [B,C,D,E,A]
