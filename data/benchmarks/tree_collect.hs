-- NOTE: it works better to bring foldList in scope instead of append
-- The problem seems to be that append has a lower weight than foldTree, due to
-- the smaller number of holes, which means that many hole fillings are tried
-- that start with append rather than foldTree. It still looks different than
-- you would expect though...
import Prelude (foldTree, foldList)
-- import Prelude (foldTree, append)

-- NOTE: this needs at least a ND of 32
-- (probably roughly 2^x, where x is the number of examples)
collect :: Tree a -> List a
collect = {}

-- NOTE: these examples are a bit different from Smyth
assert collect Leaf                                           <== []
assert collect (Node Leaf A Leaf)                             <== [A]
assert collect (Node (Node Leaf A Leaf) B Leaf)               <== [A, B]
assert collect (Node Leaf A (Node Leaf B Leaf))               <== [A, B]
assert collect (Node (Node Leaf A Leaf) B (Node Leaf C Leaf)) <== [A, B, C]
