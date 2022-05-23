import Prelude (elimBool)

or :: Bool -> Bool -> Bool
or x y = {}

assert or \True True -> True
assert or \True False -> True
assert or \False True -> True
assert or \False False -> False
