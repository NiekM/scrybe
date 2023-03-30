{-# INCLUDE foldr #-}

{-# DESC "All but the first element of a list" #-}
tail :: List a -> List a
tail = _

assert tail []        <== []
assert tail [A]       <== []
assert tail [A, B]    <== [B]
assert tail [C, D, E] <== [D, E]
