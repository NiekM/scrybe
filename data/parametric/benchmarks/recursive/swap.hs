{-# INCLUDE foldr #-}

{-# DESC "Swap the elements in a list pairwise" #-}
swap :: List a -> List a
swap = _

assert swap []        <== []
assert swap [A,B]     <== [B,A]
assert swap [A,B,C,D] <== [B,A,D,C]
