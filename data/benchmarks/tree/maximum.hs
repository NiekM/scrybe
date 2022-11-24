{-# INCLUDE mapTree, foldTree #-}
{-# INCLUDE max #-}

{-# DESC "The largest number in a tree" #-}
maximum :: Tree Nat -> Nat
maximum = _

assert maximum Leaf                                           <== 0
assert maximum (Node Leaf 1 Leaf)                             <== 1
assert maximum (Node (Node Leaf 2 Leaf) 1 Leaf)               <== 2
assert maximum (Node Leaf 0 (Node Leaf 1 Leaf))               <== 1
assert maximum (Node (Node Leaf 0 Leaf) 1 (Node Leaf 0 Leaf)) <== 1
assert maximum (Node (Node Leaf 1 (Node Leaf 3 Leaf)) 2 Leaf) <== 3
