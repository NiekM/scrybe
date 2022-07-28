-- {-# INCLUDE paraList, elimOrd, compareNat #-}
{-# INCLUDE elimBool, leq #-}

-- NOTE: using compareNat and elimOrd is considerably slower
-- {-# INCLUDE elimOrd, compareNat #-}

insert :: Nat -> List Nat -> List Nat
-- insert = _
insert n = paraList _ _

assert insert 0 []     <== [0]
assert insert 1 []     <== [1]
assert insert 2 []     <== [2]
assert insert 0 [0]    <== [0, 0]
assert insert 1 [0]    <== [0, 1]
assert insert 0 [1]    <== [0, 1]
assert insert 1 [1]    <== [1, 1]
assert insert 2 [1]    <== [1, 2]
assert insert 0 [2]    <== [0, 2]
assert insert 1 [2]    <== [1, 2]
assert insert 0 [0, 1] <== [0, 0, 1]
assert insert 2 [0, 1] <== [0, 1, 2]
