{-# INCLUDE foldList :: (c -> b) -> (a -> (c -> b) -> c -> b) -> List a -> c -> b #-}
{-# INCLUDE elimNat #-}

take :: Nat -> List a -> List a
take = _

assert take 0 []        <== []
assert take 0 [1]       <== []
assert take 1 []        <== []
assert take 1 [1]       <== [1]
assert take 1 [0, 1]    <== [0]
assert take 2 [1]       <== [1]
assert take 2 [1, 0, 1] <== [1, 0]
