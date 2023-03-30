{-# INCLUDE foldr, elimList #-}

{-# DESC "Map a function over a list" #-}
map :: (a -> b) -> List a -> List b
map = _

assert map coerceDummy []     <== []
assert map coerceDummy [A]    <== [V]
assert map coerceDummy [A, B] <== [V, W]
assert map coerceDummy [B]    <== [W]
assert map coerceDummy [C, D] <== [X, Y]
