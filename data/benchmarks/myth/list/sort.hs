import Prelude (foldList, insert)

sort :: List Nat -> List Nat
sort = {}

assert sort []      <== []
assert sort [1]     <== [1]
assert sort [2,1]   <== [1,2]
assert sort [0,0]   <== [0,0]
assert sort [1,0,0] <== [0,0,1]
assert sort [2,3,1] <== [1,2,3]
