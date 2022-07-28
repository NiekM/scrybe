{-# INCLUDE foldList :: (c -> b) -> (a -> (c -> b) -> c -> b) -> List a -> c -> b #-}
{-# INCLUDE elimNat #-}

take :: Nat -> List a -> List a
take = _

assert take 0 []        <== []
assert take 0 [A]       <== []
assert take 1 []        <== []
assert take 1 [A]       <== [A]
assert take 1 [A, B]    <== [A]
assert take 2 [A]       <== [A]
assert take 2 [A, B, C] <== [A, B]
