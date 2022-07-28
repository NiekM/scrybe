{-# INCLUDE elimNat, append #-}

level :: Nat -> Tree a -> List a
level n t = foldTree _ _ t n

assert level 0 Leaf <== []
assert level 1 Leaf <== []
assert level 0 (Node Leaf A Leaf) <== [A]
assert level 1 (Node Leaf A Leaf) <== []
assert level 1 (Node (Node Leaf A Leaf) B Leaf) <== [A]
assert level 0 (Node (Node Leaf A Leaf) B (Node Leaf C Leaf)) <== [B]
assert level 1 (Node (Node Leaf A Leaf) B (Node Leaf C Leaf)) <== [A, C]
assert level 2 (Node (Node Leaf A Leaf) B (Node Leaf C Leaf)) <== []
assert level 2 (Node Leaf A (Node Leaf B (Node Leaf C Leaf))) <== [C]
