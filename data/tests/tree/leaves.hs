{-# INCLUDE foldTree, plus #-}

{-# DESC "The number of leaves in a tree" #-}
leaves :: Tree a -> Nat
leaves = _

assert leaves Leaf                                           <== 1
assert leaves (Node Leaf A Leaf)                             <== 2
assert leaves (Node (Node Leaf A Leaf) B Leaf)               <== 3
assert leaves (Node Leaf A (Node Leaf B Leaf))               <== 3
assert leaves (Node (Node Leaf A Leaf) B (Node Leaf C Leaf)) <== 4
