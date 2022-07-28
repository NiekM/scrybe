{-# INCLUDE foldList :: (c -> b) -> (a -> (c -> b) -> c -> b) -> List a -> c -> b #-}
{-# INCLUDE elimNat #-}

drop :: Nat -> List a -> List a
drop = _

assert drop 0 []        <== []
assert drop 0 [1]       <== [1]
assert drop 0 [1, 0, 1] <== [1, 0, 1]
assert drop 1 []        <== []
assert drop 1 [1]       <== []
assert drop 1 [0, 1]    <== [1]
assert drop 1 [1, 0, 1] <== [0, 1]
assert drop 2 [1]       <== []
assert drop 2 [1, 0, 1] <== [1]
