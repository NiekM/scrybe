import Prelude (foldTree)

inc :: Tree Nat -> Tree Nat
inc = _

assert inc Leaf                                           <== Leaf
assert inc (Node Leaf 1 Leaf)                             <== Node Leaf 2 Leaf
assert inc (Node (Node Leaf 2 Leaf) 1 Leaf)               <== Node (Node Leaf 3 Leaf) 2 Leaf
assert inc (Node Leaf 0 (Node Leaf 1 Leaf))               <== Node Leaf 1 (Node Leaf 2 Leaf)
assert inc (Node (Node Leaf 0 Leaf) 1 (Node Leaf 0 Leaf)) <== Node (Node Leaf 1 Leaf) 2 (Node Leaf 1 Leaf)
assert inc (Node (Node Leaf 1 (Node Leaf 3 Leaf)) 2 Leaf) <== Node (Node Leaf 2 (Node Leaf 4 Leaf)) 3 Leaf
