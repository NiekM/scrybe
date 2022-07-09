import Prelude (foldListIndexed, elimNat)
-- import Prelude (elimNat)

-- TODO: how to handle this recursive pattern nicely?
-- NOTE: this is really similar to index.
drop :: Nat -> List a -> List a
-- drop n xs = foldList {} {} xs n
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
