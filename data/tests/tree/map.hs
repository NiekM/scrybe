{-# INCLUDE foldTree #-}

{-# DESC "Map a function over a tree" #-}
map :: (a -> b) -> Tree a -> Tree b
map = _

assert map succ Leaf                             <== Leaf
assert map succ (Node Leaf 0 Leaf)               <== Node Leaf 1 Leaf
assert map succ (Node (Node Leaf 2 Leaf) 1 Leaf) <== Node (Node Leaf 3 Leaf) 2 Leaf
assert map succ (Node Leaf 1 (Node Leaf 0 Leaf)) <== Node Leaf 2 (Node Leaf 1 Leaf)
