import Prelude (foldList, append)

concat :: List (List a) -> List a
concat = {}

assert concat \[] -> []
assert concat \[[]] -> []
assert concat \[[0]] -> [0]

-- NOTE: this constraint was missing from Smyth
assert concat \[[0], [1]] -> [0, 1]
-- ALTERNATIVE:
-- assert concat \[[A], [B]] -> [A, B]

-- NOTE: these constraints are not really necessary from Smyth
-- assert concat \[[0], [0]] -> [0, 0]
-- assert concat \[[1]] -> [1]
-- assert concat \[[1], [1]] -> [1, 1]
