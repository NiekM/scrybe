{-# INCLUDE elimNat, foldr :: (a -> (c -> b) -> c -> b) -> (c -> b) -> List a -> c -> b #-}

{-# DESC "All but the first `n` elements of a list" #-}
drop :: Nat -> List a -> List a
drop = _

assert drop 0 []        <== []
assert drop 0 [A, B, C] <== [A, B, C]
assert drop 1 [A]       <== []
assert drop 1 [A, B]    <== [B]
assert drop 2 [A]       <== []
assert drop 2 [A, B, C] <== [C]
