{-# INCLUDE foldTree, plus #-}

sum :: Tree Nat -> Nat
sum = _

assert sum Leaf                                           <== 0
assert sum (Node Leaf 1 Leaf)                             <== 1
assert sum (Node (Node Leaf 2 Leaf) 1 Leaf)               <== 3
assert sum (Node Leaf 0 (Node Leaf 1 Leaf))               <== 1
assert sum (Node (Node Leaf 0 Leaf) 1 (Node Leaf 0 Leaf)) <== 1
assert sum (Node (Node Leaf 1 (Node Leaf 3 Leaf)) 2 Leaf) <== 6
