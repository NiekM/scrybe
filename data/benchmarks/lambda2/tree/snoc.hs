import Prelude (foldTree, foldList)

cons :: a -> Tree (List a) -> Tree (List a)
cons = _

assert cons A Leaf                                                  <== Leaf
assert cons A (Node Leaf [] Leaf)                                   <== Node Leaf [A] Leaf
assert cons A (Node (Node Leaf [B] Leaf) [] Leaf)                   <== Node (Node Leaf [B,A] Leaf) [A] Leaf
assert cons A (Node Leaf [A] (Node Leaf [B] Leaf))                  <== Node Leaf [A,A] (Node Leaf [B,A] Leaf)
assert cons A (Node (Node Leaf [] Leaf) [] (Node Leaf [] Leaf))     <== Node (Node Leaf [A] Leaf) [A] (Node Leaf [A] Leaf)
assert cons A (Node (Node Leaf [D] (Node Leaf [B,C] Leaf)) [] Leaf) <== Node (Node Leaf [D,A] (Node Leaf [B,C,A] Leaf)) [A] Leaf
