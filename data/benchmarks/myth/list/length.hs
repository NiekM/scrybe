import Prelude (foldList)

length :: List a -> Nat
length = {}

assert length []     <== 0
assert length [0]    <== 1
assert length [0, 0] <== 2
