import Prelude (foldTree, plus)

size :: Tree a -> Nat
size = _

assert size Leaf                                           <== 0
assert size (Node Leaf A Leaf)                             <== 1
assert size (Node (Node Leaf A Leaf) B Leaf)               <== 2
assert size (Node Leaf A (Node Leaf B Leaf))               <== 2
assert size (Node (Node Leaf A Leaf) B (Node Leaf C Leaf)) <== 3
