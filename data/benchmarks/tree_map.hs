import Prelude (foldTree)

map :: (a -> b) -> Tree a -> Tree b
map = {}

-- NOTE: these examples are a bit different from Smyth
assert map \Succ Leaf -> Leaf
assert map \Succ (Node Leaf 0 Leaf) -> Node Leaf 1 Leaf
assert map \Succ (Node (Node Leaf 2 Leaf) 1 Leaf) -> Node (Node Leaf 3 Leaf) 2 Leaf
assert map \Succ (Node Leaf 1 (Node Leaf 0 Leaf)) -> Node Leaf 2 (Node Leaf 1 Leaf)
