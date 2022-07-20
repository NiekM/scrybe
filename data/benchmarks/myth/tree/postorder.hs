-- import Prelude (append)
import Prelude (foldTree, append)

postorder :: Tree a -> List a
-- postorder t = foldTree [] (\l x r -> foldTree {} (\a b c -> {}) t) t
-- postorder t = foldTree {} {} t
postorder = {}

assert postorder Leaf                                           <== []
assert postorder (Node Leaf A Leaf)                             <== [A]
assert postorder (Node (Node Leaf A Leaf) B Leaf)               <== [A, B]
assert postorder (Node Leaf A (Node Leaf B Leaf))               <== [B, A]
assert postorder (Node (Node Leaf A Leaf) B (Node Leaf C Leaf)) <== [A, C, B]
