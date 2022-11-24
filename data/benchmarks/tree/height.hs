{-# INCLUDE mapTree, foldTree #-}
{-# INCLUDE max #-}

{-# DESC "The height of a tree" #-}
height :: Tree a -> Nat
height = _

assert height Leaf                                           <== 0
assert height (Node Leaf A Leaf)                             <== 1
assert height (Node (Node Leaf A Leaf) B Leaf)               <== 2
assert height (Node Leaf A (Node Leaf B Leaf))               <== 2
assert height (Node (Node Leaf A Leaf) B (Node Leaf C Leaf)) <== 2
assert height (Node (Node Leaf A (Node Leaf C Leaf)) B Leaf) <== 3
