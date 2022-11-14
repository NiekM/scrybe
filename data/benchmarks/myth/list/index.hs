-- {-# INCLUDE foldList :: (c -> b) -> (a -> (c -> b) -> c -> b) -> List a -> c -> b #-}
{-# INCLUDE foldr :: (a -> (c -> b) -> c -> b) -> (c -> b) -> List a -> c -> b #-}
{-# INCLUDE elimNat #-}

index :: Nat -> List a -> Maybe a
index = _

assert index 0 []        <== Nothing
assert index 1 []        <== Nothing
assert index 0 [A]       <== Just A
assert index 1 [A]       <== Nothing
assert index 0 [A, B]    <== Just A
assert index 1 [A, B, C] <== Just B
assert index 2 [A, B, C] <== Just C
