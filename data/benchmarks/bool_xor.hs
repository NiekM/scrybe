import Prelude (elimBool)

-- TODO: xor needs 2 elimBools
xor :: Bool -> Bool -> Bool
xor = {}

assert xor \True  True  -> False
assert xor \True  False -> True
assert xor \False True  -> True
assert xor \False False -> False
