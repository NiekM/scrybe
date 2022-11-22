-- {-# INCLUDE elimBool, leq #-}
{-# INCLUDE elimBool #-}
-- {-# INCLUDE paraList #-}
{-# INCLUDE paraList :: List b -> (a -> List a -> List b -> List b) -> List a -> List b #-}

-- NOTE: using compareNat and elimOrd is considerably slower
-- {-# INCLUDE paraList, elimOrd, compareNat #-}

insert :: (a -> a -> Bool) -> a -> List a -> List a
-- insert n = paraList [n] (\x xs r -> _)
insert = _

assert insert leq 0 []     <== [0]
-- assert insert leq 1 []     <== [1]
assert insert leq 2 []     <== [2]
assert insert leq 0 [0]    <== [0, 0]
assert insert leq 1 [0]    <== [0, 1]
assert insert leq 0 [1]    <== [0, 1]
assert insert leq 1 [1]    <== [1, 1]
assert insert leq 2 [1]    <== [1, 2]
-- assert insert leq 0 [2]    <== [0, 2]
assert insert leq 1 [2]    <== [1, 2]
assert insert leq 0 [0, 1] <== [0, 0, 1]
assert insert leq 2 [0, 1] <== [0, 1, 2]
-- assert insert leq 2 [0,1,3] <== [0,1,2,3]
