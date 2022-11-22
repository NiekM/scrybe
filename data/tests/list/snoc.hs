{-# INCLUDE foldr #-}

{-# DESC "Add an element to the end of a list" #-}
snoc :: List a -> a -> List a
snoc = _

assert snoc []  A       <== [A]
assert snoc [A] A       <== [A, A]
assert snoc [A] B       <== [A, B]
assert snoc [A, B] C    <== [A, B, C]
assert snoc [A, B, C] D <== [A, B, C, D]
