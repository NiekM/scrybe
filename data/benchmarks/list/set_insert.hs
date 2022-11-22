{-# INCLUDE paraList, elimOrd, compareNat #-}

{-# DESC "Insert an element in a set" #-}
set_insert :: Nat -> List Nat -> List Nat
set_insert n = _

assert set_insert 0 []              <== [0]
assert set_insert 0 [0]             <== [0]
assert set_insert 1 [0]             <== [0, 1]
assert set_insert 0 [1]             <== [0, 1]
assert set_insert 1 [1]             <== [1]
assert set_insert 1 [0]             <== [0, 1]
assert set_insert 0 [2]             <== [0, 2]
assert set_insert 0 [0, 1]          <== [0, 1]
assert set_insert 2 [0, 1]          <== [0, 1, 2]
assert set_insert 2 [0, 1, 3]       <== [0, 1, 2, 3]
assert set_insert 2 [0, 1, 2]       <== [0, 1, 2]
assert set_insert 2 [0, 1, 2, 3, 4] <== [0, 1, 2, 3, 4]
