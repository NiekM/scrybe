import Prelude (foldListIndexed, elimNat)

index :: Nat -> List a -> Maybe a
index = _

-- assert index 0 []        <== Nothing
-- assert index 0 [1]       <== Just 1
-- assert index 0 [2]       <== Just 2
-- assert index 0 [1, 2]    <== Just 1
-- assert index 0 [2, 1]    <== Just 2
-- assert index 0 [3, 2, 1] <== Just 3
-- assert index 1 []        <== Nothing
-- assert index 1 [1]       <== Nothing
-- assert index 1 [2]       <== Nothing
-- assert index 1 [1, 2]    <== Just 2
-- assert index 1 [2, 1]    <== Just 1
-- assert index 1 [3, 2, 1] <== Just 2
-- assert index 2 [3, 2, 1] <== Just 1

-- This smaller set of constraints is considerably faster
assert index 0 []        <== Nothing
assert index 0 [1]       <== Just 1
assert index 0 [3, 2, 1] <== Just 3
assert index 1 []        <== Nothing
assert index 1 [2]       <== Nothing
assert index 1 [3, 2, 1] <== Just 2
assert index 2 [3, 2, 1] <== Just 1
