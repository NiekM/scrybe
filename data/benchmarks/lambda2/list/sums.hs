import Prelude (map, foldList, plus)

sums :: List (List Nat) -> List Nat
sums = _

assert sums [] <== []
assert sums [[]] <== [0]
assert sums [[1, 2]] <== [3]
assert sums [[0], [2]] <== [0, 2]
assert sums [[2, 3], [3]] <== [5, 3]
