import Prelude (elimBool)

xor :: Bool -> Bool -> Bool
xor = _

assert xor True  True  <== False
assert xor True  False <== True
assert xor False True  <== True
assert xor False False <== False
