import Prelude (foldTree, append)

inorder :: Tree a -> List a
inorder = {}

assert inorder Leaf                                           <== []
assert inorder (Node Leaf A Leaf)                             <== [A]
assert inorder (Node (Node Leaf A Leaf) B Leaf)               <== [A, B]
assert inorder (Node Leaf A (Node Leaf B Leaf))               <== [A, B]
assert inorder (Node (Node Leaf A Leaf) B (Node Leaf C Leaf)) <== [A, B, C]
