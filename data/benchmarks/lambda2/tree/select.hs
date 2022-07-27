import Prelude (foldTree, append, elimBool)
-- import Prelude (foldTree, append, filter)

select :: (a -> Bool) -> Tree a -> List a
select = _

assert select even Leaf                                           <== []
assert select even (Node Leaf 0 Leaf)                             <== [0]
assert select even (Node Leaf 1 Leaf)                             <== []
assert select even (Node (Node Leaf 1 Leaf) 0 Leaf)               <== [0]
assert select even (Node (Node Leaf 0 Leaf) 0 Leaf)               <== [0, 0]
assert select even (Node Leaf 2 (Node Leaf 4 Leaf))               <== [2, 4]
assert select even (Node (Node Leaf 1 Leaf) 3 (Node Leaf 1 Leaf)) <== []
assert select even (Node (Node Leaf 1 Leaf) 3 (Node Leaf 2 Leaf)) <== [2]
