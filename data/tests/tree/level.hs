{-# INCLUDE foldTreeN, plus #-}

{-# DESC "The number of nodes at depth `n`" #-}
level :: Nat -> Tree a -> Nat
level = _

assert level 0 Leaf <== 0
assert level 1 Leaf <== 0
assert level 0 (Node Leaf A Leaf) <== 1
assert level 1 (Node Leaf A Leaf) <== 0
assert level 1 (Node (Node Leaf A Leaf) B Leaf) <== 1
assert level 0 (Node (Node Leaf A Leaf) B (Node Leaf C Leaf)) <== 1
assert level 1 (Node (Node Leaf A Leaf) B (Node Leaf C Leaf)) <== 2
assert level 2 (Node (Node Leaf A Leaf) B (Node Leaf C Leaf)) <== 0
assert level 2 (Node Leaf A (Node Leaf B (Node Leaf C Leaf))) <== 1
