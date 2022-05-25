import Prelude (elimBool)

and :: Bool -> Bool -> Bool
and = {}

assert and \True  True  -> True
assert and \True  False -> False
assert and \False True  -> False
assert and \False False -> False
