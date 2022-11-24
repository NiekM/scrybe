{-# INCLUDE foldTree, elimTree #-}
{-# INCLUDE append #-}

{-# DESC "Inorder traversal of a tree" #-}
inorder :: Tree a -> List a
inorder = _

assert inorder Leaf                                           <== []
assert inorder (Node Leaf A Leaf)                             <== [A]
assert inorder (Node (Node Leaf A Leaf) B Leaf)               <== [A, B]
assert inorder (Node Leaf A (Node Leaf B Leaf))               <== [A, B]
assert inorder (Node (Node Leaf A Leaf) B (Node Leaf C Leaf)) <== [A, B, C]
