import Prelude (foldList)

length :: List a -> Nat
length = {}

assert length \[] -> 0
assert length \[3,3] -> 2
assert length \[1,2,3] -> 3
