import Prelude (foldList, elimMaybe)

last :: List a -> Maybe a
last = {}

assert last []        <== Nothing
assert last [1]       <== Just 1
assert last [2]       <== Just 2
assert last [2, 1]    <== Just 1

-- NOTE: less assertions seems to be way faster

-- assert last [1, 2]    <== Just 2
-- assert last [3, 2, 1] <== Just 1
