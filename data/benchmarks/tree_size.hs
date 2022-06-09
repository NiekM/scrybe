import Prelude (foldTree, plus)

collect :: Tree a -> Nat
collect = {}

-- NOTE: these examples are a bit different from Smyth
assert collect Leaf                                           <== 0
assert collect (Node Leaf A Leaf)                             <== 1
assert collect (Node (Node Leaf A Leaf) B Leaf)               <== 2
assert collect (Node Leaf A (Node Leaf B Leaf))               <== 2
assert collect (Node (Node Leaf A Leaf) B (Node Leaf C Leaf)) <== 3
