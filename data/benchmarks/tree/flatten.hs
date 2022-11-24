{-# INCLUDE map, filter, foldl, foldr, elimList #-}
{-# INCLUDE mapTree, foldTree #-}
{-# INCLUDE append #-}

{-# DESC "Flatten a tree of lists into a list" #-}
flatten :: Tree (List a) -> List a
flatten = _

assert flatten Leaf                                                 <== []
assert flatten (Node Leaf [] Leaf)                                  <== []
assert flatten (Node Leaf [A] Leaf)                                 <== [A]
assert flatten (Node (Node Leaf [A] Leaf) [B,C] Leaf)               <== [A, B, C]
assert flatten (Node Leaf [A] (Node Leaf [B] Leaf))                 <== [A, B]
assert flatten (Node Leaf [A,B,C] (Node Leaf [D,E] Leaf))           <== [A, B, C, D, E]
assert flatten (Node (Node Leaf [A] Leaf) [] (Node Leaf [B] Leaf))  <== [A, B]
assert flatten (Node (Node Leaf [A] Leaf) [B] (Node Leaf [C] Leaf)) <== [A, B, C]
assert flatten (Node (Node Leaf [] Leaf) [A,B] (Node Leaf [] Leaf)) <== [A, B]
