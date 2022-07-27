import Prelude (map, plus)

add :: Nat -> List Nat -> List Nat
add = {}

assert add 3 [] <== []
assert add 1 [1, 2] <== [2, 3]
assert add 2 [0, 0] <== [2, 2]
assert add 0 [3, 4, 5] <== [3, 4, 5]
