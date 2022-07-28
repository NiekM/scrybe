{-# INCLUDE elimNat, append #-}

insert :: Nat -> Tree Nat -> List Nat
insert n = foldTree (Node Leaf n Leaf) _

assert insert 0 Leaf <== Node Leaf 0 Leaf
assert insert 1 Leaf <== Node Leaf 1 Leaf
assert insert 2 Leaf <== Node Leaf 2 Leaf
assert insert 0 (Node Leaf 0 Leaf) <== Node Leaf 0 Leaf
assert insert 1 (Node Leaf 0 Leaf) <== Node Leaf 0 (Node Leaf 1 Leaf)
assert insert 2 (Node Leaf 0 Leaf) <== Node Leaf 0 (Node Leaf 2 Leaf)
assert insert 0 (Node Leaf 1 Leaf) <== Node Leaf 0 (Node Leaf 1 Leaf)
assert insert 1 (Node Leaf 1 Leaf) <== Node Leaf 1 Leaf
assert insert 2 (Node Leaf 1 Leaf) <== Node Leaf 1 (Node Leaf 2 Leaf)
assert insert 0 (Node Leaf 2 Leaf) <== Node Leaf 0 (Node Leaf 2 Leaf)
assert insert 1 (Node Leaf 2 Leaf) <== Node Leaf 1 (Node Leaf 2 Leaf)
assert insert 2 (Node Leaf 2 Leaf) <== Node Leaf 2 Leaf
