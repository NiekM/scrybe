{-# INCLUDE map, filter, foldl, foldr, elimList #-}
{-# INCLUDE mapTree, foldTree #-}
{-# INCLUDE plus #-}

{-# DESC "The sum of each list in a tree of lists" #-}
sum_lists :: Tree (List Nat) -> Tree Nat
sum_lists = _

assert sum_lists Leaf                                                <== Leaf
assert sum_lists (Node Leaf [] Leaf)                                 <== Node Leaf 0 Leaf
assert sum_lists (Node Leaf [1] Leaf)                                <== Node Leaf 1 Leaf
assert sum_lists (Node (Node Leaf [2] Leaf) [] Leaf)                 <== Node (Node Leaf 2 Leaf) 0 Leaf
assert sum_lists (Node (Node Leaf [1, 2] Leaf) [1, 1, 1] Leaf)       <== Node (Node Leaf 3 Leaf) 3 Leaf
assert sum_lists (Node Leaf [1] (Node Leaf [] Leaf))                 <== Node Leaf 1 (Node Leaf 0 Leaf)
assert sum_lists (Node (Node Leaf [] Leaf) [1] (Node Leaf [0] Leaf)) <== Node (Node Leaf 0 Leaf) 1 (Node Leaf 0 Leaf)
