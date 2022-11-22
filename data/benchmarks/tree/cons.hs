{-# INCLUDE foldTree #-}

{-# DESC "Add an element to the front of each node in a tree of lists" #-}
cons :: a -> Tree (List a) -> Tree (List a)
cons = _

assert cons A Leaf                                                  <== Leaf
assert cons A (Node Leaf [] Leaf)                                   <== Node Leaf [A] Leaf
assert cons A (Node (Node Leaf [B] Leaf) [] Leaf)                   <== Node (Node Leaf [A,B] Leaf) [A] Leaf
assert cons A (Node Leaf [A] (Node Leaf [B] Leaf))                  <== Node Leaf [A,A] (Node Leaf [A,B] Leaf)
assert cons A (Node (Node Leaf [] Leaf) [] (Node Leaf [] Leaf))     <== Node (Node Leaf [A] Leaf) [A] (Node Leaf [A] Leaf)
assert cons A (Node (Node Leaf [D] (Node Leaf [B,C] Leaf)) [] Leaf) <== Node (Node Leaf [A,D] (Node Leaf [A,B,C] Leaf)) [A] Leaf
