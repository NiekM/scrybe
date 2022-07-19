import Prelude (foldListIndexed, elimNat)

drop :: Nat -> List a -> List a
drop = {}

-- This smaller set of constraints is faster, but does not give the same set of
-- solutions.
assert drop 0 []        <== []
assert drop 0 [1]       <== [1]
assert drop 0 [1, 0, 1] <== [1, 0, 1]
assert drop 1 []        <== []
assert drop 1 [1]       <== []
assert drop 1 [0, 1]    <== [1]
assert drop 1 [1, 0, 1] <== [0, 1]
assert drop 2 [1]       <== []
assert drop 2 [1, 0, 1] <== [1]
