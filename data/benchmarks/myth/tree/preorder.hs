{-# INCLUDE foldTree, append #-}

preorder :: Tree a -> List a
preorder = _

assert preorder Leaf                                           <== []
assert preorder (Node Leaf A Leaf)                             <== [A]
assert preorder (Node (Node Leaf A Leaf) B Leaf)               <== [B, A]
assert preorder (Node Leaf A (Node Leaf B Leaf))               <== [A, B]
assert preorder (Node (Node Leaf A Leaf) B (Node Leaf C Leaf)) <== [B, A, C]
