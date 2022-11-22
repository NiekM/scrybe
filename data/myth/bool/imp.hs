{-# INCLUDE elimBool #-}

imp :: Bool -> Bool -> Bool
imp = _

assert imp True  True  <== True
assert imp True  False <== False
assert imp False True  <== True
assert imp False False <== True
