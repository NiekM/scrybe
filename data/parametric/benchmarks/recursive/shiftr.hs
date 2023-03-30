{-# INCLUDE map, filter, foldl, foldr, elimList #-}
{-# INCLUDE reverse #-}

{-# DESC "Shift all elements in a list to the right" #-}
shiftr :: List a -> List a
shiftr = _

assert shiftr []          <== []
assert shiftr [A]         <== [A]
assert shiftr [A,B]       <== [B,A]
assert shiftr [A,B,C]     <== [C,A,B]
assert shiftr [A,B,C,D]   <== [D,A,B,C]
assert shiftr [A,B,C,D,E] <== [E,A,B,C,D]
