-- {-# INCLUDE elimNat, plus #-}
-- {-# INCLUDE elimNat, foldNat #-}
-- {-# INCLUDE foldNat #-}
{-# INCLUDE plus, elimNat #-}

level :: Nat -> Tree a -> Nat
-- level n t = foldTree _ _ t n
-- level n t = foldTree (const 0) (\l x r -> elimNat 1 (\m -> plus (l m) (r m))) t n
-- level n t = foldTree _ (\l x r -> elimNat _ (\m -> plus _ _)) t n
-- level n t = foldTree _ (\l x r -> elimNat _ (\m -> _)) t n
level n t = foldTree _ (\l x r m -> _) t n

assert level 0 Leaf <== 0
assert level 1 Leaf <== 0
assert level 2 Leaf <== 0
assert level 0 (Node Leaf A Leaf) <== 1
assert level 1 (Node Leaf A Leaf) <== 0
assert level 1 (Node (Node Leaf A Leaf) B Leaf) <== 1
assert level 0 (Node (Node Leaf A Leaf) B (Node Leaf C Leaf)) <== 1
assert level 1 (Node (Node Leaf A Leaf) B (Node Leaf C Leaf)) <== 2
assert level 2 (Node (Node Leaf A Leaf) B (Node Leaf C Leaf)) <== 0
assert level 2 (Node Leaf A (Node Leaf B (Node Leaf C Leaf))) <== 1
