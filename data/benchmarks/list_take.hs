import Prelude (foldList, elimNat)

-- TODO: somewhere this also reaches a diverging unevaluation.
take :: Nat -> List a -> List a
take = {}

assert take \0 [] -> []
assert take \0 [1] -> []
assert take \0 [0, 1] -> []
assert take \0 [1, 0, 1] -> []
assert take \1 [] -> []
assert take \1 [1] -> [1]
assert take \1 [0, 1] -> [0]
assert take \1 [1, 0, 1] -> [1]
assert take \2 [] -> []
assert take \2 [1] -> [1]
assert take \2 [0, 1] -> [0, 1]
assert take \2 [1, 0, 1] -> [1, 0]
