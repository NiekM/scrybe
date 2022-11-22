{-# INCLUDE foldTree, or, elem #-}

{-# DESC "Whether a number occurs in a tree of lists" #-}
search :: Nat -> Tree (List Nat) -> Bool
search = _

assert search 0 Leaf                                                <== False
assert search 0 (Node Leaf [] Leaf)                                 <== False
assert search 0 (Node Leaf [1] Leaf)                                <== False
assert search 1 (Node Leaf [1] Leaf)                                <== True
assert search 0 (Node (Node Leaf [2] Leaf) [] Leaf)                 <== False
assert search 0 (Node (Node Leaf [0] Leaf) [] Leaf)                 <== True
assert search 0 (Node (Node Leaf [1, 2] Leaf) [1, 1, 1] Leaf)       <== False
assert search 2 (Node (Node Leaf [1, 2] Leaf) [1, 1, 1] Leaf)       <== True
assert search 0 (Node Leaf [1] (Node Leaf [] Leaf))                 <== False
assert search 1 (Node Leaf [1] (Node Leaf [] Leaf))                 <== True
assert search 0 (Node (Node Leaf [] Leaf) [1] (Node Leaf [0] Leaf)) <== True
assert search 0 (Node (Node Leaf [0] Leaf) [1] (Node Leaf [] Leaf)) <== True
assert search 2 (Node (Node Leaf [0] Leaf) [1] (Node Leaf [] Leaf)) <== False
