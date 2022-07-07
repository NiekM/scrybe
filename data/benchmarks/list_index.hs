import Prelude (elimNat)

-- MODEL:
-- index n xs = foldList (const Nothing) (\x r m -> elimNat (Just x) r m) xs n

-- TODO: how to handle this recursive pattern nicely?
-- NOTE: this is really similar to take.
index :: Nat -> List a -> Maybe a
-- NOTE: this diverges, but synthesis also does not know how to introduce the
-- correct recursion scheme.
-- index = {}
index n xs = foldList {} {} xs n
-- NOTE: this version does not adhere to the informativeness restriction
-- index n xs = foldList {} {} xs {}

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
