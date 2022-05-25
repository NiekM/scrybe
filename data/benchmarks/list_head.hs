import Prelude (elimList)

head :: List a -> Maybe a
head = {}

assert head \[]            -> Nothing
assert head \[1]           -> Just 1
assert head \[True, False] -> Just True
assert head \[0, 1, 2]     -> Just 0
