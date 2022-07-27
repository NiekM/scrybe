import Prelude (map)

incs :: List (List Nat) -> List (List Nat)
incs = _

assert incs [] <== []
assert incs [[]] <== [[]]
assert incs [[1, 2]] <== [[2, 3]]
assert incs [[0], [0]] <== [[1], [1]]
assert incs [[3, 4], [5]] <== [[4, 5], [6]]
