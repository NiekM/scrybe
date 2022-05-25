import Prelude (elimMaybe)

last :: List a -> Maybe a
-- last = {} -- NOTE: this one (with foldList in scope) diverges
last = foldList {} {}

assert last \[]        -> Nothing
assert last \[1]       -> Just 1
assert last \[2]       -> Just 2
assert last \[2, 1]    -> Just 1
assert last \[1, 2]    -> Just 2
assert last \[3, 2, 1] -> Just 1
