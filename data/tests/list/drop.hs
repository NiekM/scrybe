-- {-# INCLUDE foldr :: (a -> (c -> b) -> c -> b) -> (c -> b) -> List a -> c -> b #-}
-- {-# INCLUDE elimNat #-}
{-# INCLUDE foldrN #-}

{-# DESC "All but the first `n` elements of a list" #-}
drop :: Nat -> List a -> List a
drop = _

assert drop 0 []        <== []
assert drop 0 [A, B, C] <== [A, B, C]
assert drop 1 [A]       <== []
assert drop 1 [A, B]    <== [B]
assert drop 2 [A]       <== []
assert drop 2 [A, B, C] <== [C]

-- NOTE: assert is still accurate and way faster with fewer examples, this
-- implies that slowly extending the example set is very beneficial.

-- assert drop 0 [A]       <== [A]
-- assert drop 1 []        <== []
-- assert drop 1 [A, B, C] <== [B, C]
