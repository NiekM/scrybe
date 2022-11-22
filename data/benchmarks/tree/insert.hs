{-# INCLUDE paraTree, compareNat, elimNat #-}

{-# DESC "Insert an element in a binary tree" #-}
insert :: Nat -> Tree Nat -> Tree Nat
insert = _

assert insert 0 Leaf                                           <== Node Leaf 0 Leaf
assert insert 1 Leaf                                           <== Node Leaf 1 Leaf
assert insert 0 (Node Leaf 1 Leaf)                             <== Node (Node Leaf 0 Leaf) 1 Leaf
assert insert 1 (Node Leaf 1 Leaf)                             <== Node Leaf 1 Leaf
assert insert 2 (Node Leaf 1 Leaf)                             <== Node Leaf 1 (Node Leaf 2 Leaf)
assert insert 2 (Node (Node Leaf 0 Leaf) 1 (Node Leaf 3 Leaf)) <== Node (Node Leaf 0 Leaf) 1 (Node (Node Leaf 2 Leaf) 3 Leaf)
assert insert 1 (Node (Node Leaf 0 Leaf) 2 (Node Leaf 3 Leaf)) <== Node (Node Leaf 0 (Node Leaf 1 Leaf)) 2 (Node Leaf 3 Leaf)
assert insert 2 (Node (Node Leaf 0 Leaf) 2 (Node Leaf 3 Leaf)) <== Node (Node Leaf 0 Leaf) 2 (Node Leaf 3 Leaf)
