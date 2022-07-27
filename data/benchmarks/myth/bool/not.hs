import Prelude (elimBool)

neg :: Bool -> Bool
neg = _

assert neg True  <== False
assert neg False <== True
