-- import Prelude (foldTree, eq, elimBool)
import Prelude (foldTree, eq, or)

elem :: Nat -> Tree Nat -> Bool
elem = {}

assert elem 0 Leaf                                           <== False
assert elem 0 (Node Leaf 1 Leaf)                             <== False
assert elem 1 (Node Leaf 1 Leaf)                             <== True
assert elem 2 (Node (Node Leaf 2 Leaf) 1 Leaf)               <== True
assert elem 2 (Node Leaf 0 (Node Leaf 1 Leaf))               <== False
assert elem 0 (Node (Node Leaf 1 Leaf) 1 (Node Leaf 0 Leaf)) <== True
assert elem 0 (Node (Node Leaf 1 (Node Leaf 3 Leaf)) 2 Leaf) <== False
