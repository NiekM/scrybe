{-# INCLUDE foldTree, map, plus #-}

{-# DESC "The sum of each tree in a list of trees" #-}
sum_trees :: List (Tree Nat) -> List Nat
sum_trees = _

assert sum_trees []                                             <== []
assert sum_trees [Leaf]                                         <== [0]
assert sum_trees [Leaf, Leaf, Leaf]                             <== [0,0,0]
assert sum_trees [Node Leaf 1 Leaf]                             <== [1]
assert sum_trees [Leaf, Node Leaf 1 Leaf]                       <== [0,1]
assert sum_trees [Node Leaf 2 Leaf, Node Leaf 3 Leaf]           <== [2,3]
assert sum_trees [Node (Node Leaf 2 Leaf) 1 Leaf]               <== [3]
assert sum_trees [Node Leaf 0 (Node Leaf 1 Leaf)]               <== [1]
assert sum_trees [Node (Node Leaf 0 Leaf) 1 (Node Leaf 0 Leaf)] <== [1]
assert sum_trees [Node (Node Leaf 1 (Node Leaf 3 Leaf)) 2 Leaf] <== [6]
