{-# INCLUDE foldr, elimList #-}

{-# DESC "Map a function over a list" #-}
map :: (a -> b) -> List a -> List b
map = _

assert map Succ []     <== []
assert map Succ [0]    <== [1]
assert map Succ [0, 0] <== [1, 1]
assert map Succ [1]    <== [2]
assert map Succ [1, 1] <== [2, 2]
