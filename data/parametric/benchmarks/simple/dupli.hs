{-# INCLUDE concatMap #-}

{-# DESC "Duplicate each element in a list" #-}
dupli :: List a -> List a
dupli = _

assert dupli []     <== []
assert dupli [A]    <== [A, A]
assert dupli [B, C] <== [B, B, C, C]
