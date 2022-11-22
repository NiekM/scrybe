-- {-# INCLUDE foldTree, foldr #-}
-- {-# INCLUDE foldTree, snoc #-}
{-# INCLUDE mapTree, foldr #-}
-- {-# INCLUDE mapTree, snoc #-}

{-# DESC "Add an element to the end of each node in a tree of lists" #-}
snoc :: a -> Tree (List a) -> Tree (List a)
snoc = _

assert snoc A Leaf                                                  <== Leaf
assert snoc A (Node Leaf [] Leaf)                                   <== Node Leaf [A] Leaf
assert snoc A (Node (Node Leaf [B] Leaf) [] Leaf)                   <== Node (Node Leaf [B,A] Leaf) [A] Leaf
assert snoc A (Node Leaf [A] (Node Leaf [B] Leaf))                  <== Node Leaf [A,A] (Node Leaf [B,A] Leaf)
assert snoc A (Node (Node Leaf [] Leaf) [] (Node Leaf [] Leaf))     <== Node (Node Leaf [A] Leaf) [A] (Node Leaf [A] Leaf)
assert snoc A (Node (Node Leaf [D] (Node Leaf [B,C] Leaf)) [] Leaf) <== Node (Node Leaf [D,A] (Node Leaf [B,C,A] Leaf)) [A] Leaf
