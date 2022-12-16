{-# INCLUDE paraNat, mult #-}

-- NOTE: we expect the following solution, but unevaluation of mult does not
-- work well:
-- factorial = paraNat 1 \x y -> mult x (Succ y)

factorial :: Nat -> Nat
factorial = _

assert factorial 0 <== 1
assert factorial 1 <== 1
assert factorial 2 <== 2
assert factorial 3 <== 6
