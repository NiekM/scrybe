-- Since we know that swap cannot be implemented using foldr, we include no
-- functions, so that the benchmark does not diverge.

{-# DESC "Swap the elements in a list pairwise" #-}
swap :: List a -> List a
swap = _

assert swap []        <== []
assert swap [A,B]     <== [B,A]
assert swap [A,B,C,D] <== [B,A,D,C]
