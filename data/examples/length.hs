-- NOTE: synthesis takes too long if foldList is explicitly in scope
-- import Prelude (foldList)

length :: List a -> Nat
-- length xs = {}
length xs = foldList {} ({}) {}

assert length \[] -> 0
assert length \[3,3] -> 2
assert length \[1,2,3] -> 3
