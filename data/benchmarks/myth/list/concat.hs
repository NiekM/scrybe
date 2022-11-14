{-# INCLUDE foldr, append #-}

concat :: List (List a) -> List a
concat = _

assert concat []    <== []
assert concat [[]]  <== []
assert concat [[A]] <== [A]
assert concat [[A], [B]] <== [A, B]
