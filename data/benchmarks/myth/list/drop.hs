-- NOTE: non-specialized version results in a timeout unless weights are added
-- on foldList.
-- {-# INCLUDE foldList :: (c -> b) -> (a -> (c -> b) -> c -> b) -> List a -> c -> b #-}
{-# INCLUDE foldList :: (Nat -> b) -> (a -> (Nat -> b) -> Nat -> b) -> List a -> Nat -> b #-}
{-# INCLUDE elimNat #-}

drop :: Nat -> List a -> List a
drop = _

assert drop 0 []        <== []
assert drop 0 [A]       <== [A]
assert drop 0 [A, B, C] <== [A, B, C]
assert drop 1 []        <== []
assert drop 1 [A]       <== []
assert drop 1 [A, B]    <== [B]
assert drop 1 [A, B, C] <== [B, C]
assert drop 2 [A]       <== []
assert drop 2 [A, B, C] <== [C]
