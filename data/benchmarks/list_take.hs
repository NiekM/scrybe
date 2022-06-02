import Prelude (elimNat)

-- MODEL:
-- take n xs = foldList (const Nothing) (\x r m -> elimNat (Just x) r m) xs n

-- TODO: how to handle this recursive pattern nicely?
-- NOTE: this is really similar to index.
take :: Nat -> List a -> List a
-- NOTE: this diverges, but synthesis also does not know how to introduce the
-- correct recursion scheme.
-- take = {}
take n xs = foldList {} {} xs n

-- assert take 0 []        <== []
-- assert take 0 [1]       <== []
-- assert take 0 [0, 1]    <== []
-- assert take 0 [1, 0, 1] <== []
-- assert take 1 []        <== []
-- assert take 1 [1]       <== [1]
-- assert take 1 [0, 1]    <== [0]
-- assert take 1 [1, 0, 1] <== [1]
-- assert take 2 []        <== []
-- assert take 2 [1]       <== [1]
-- assert take 2 [0, 1]    <== [0, 1]
-- assert take 2 [1, 0, 1] <== [1, 0]

-- This smaller set of constraints is faster, but does not give the same set of
-- solutions.
assert take 0 []        <== []
assert take 0 [1]       <== []
assert take 1 []        <== []
assert take 1 [1]       <== [1]
assert take 1 [0, 1]    <== [0]
assert take 2 [1]       <== [1]
assert take 2 [1, 0, 1] <== [1, 0]
