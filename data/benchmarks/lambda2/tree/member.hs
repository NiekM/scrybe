-- import Prelude (foldTree, eq, elimBool)
import Prelude (foldTree, eq, or)

member :: Nat -> Tree Nat -> Bool
member = {}

assert member 0 Leaf                                           <== False
assert member 0 (Node Leaf 1 Leaf)                             <== False
assert member 1 (Node Leaf 1 Leaf)                             <== True
assert member 2 (Node (Node Leaf 2 Leaf) 1 Leaf)               <== True
assert member 2 (Node Leaf 0 (Node Leaf 1 Leaf))               <== False
assert member 0 (Node (Node Leaf 1 Leaf) 1 (Node Leaf 0 Leaf)) <== True
assert member 0 (Node (Node Leaf 1 (Node Leaf 3 Leaf)) 2 Leaf) <== False
