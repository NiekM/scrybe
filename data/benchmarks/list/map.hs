{-# INCLUDE foldr, elimList #-}

{-# DESC "Map a function over a list" #-}
map :: (a -> b) -> List a -> List b
map = _

assert map succ []     <== []
assert map succ [0]    <== [1]
assert map succ [0, 0] <== [1, 1]
assert map succ [1]    <== [2]
assert map succ [1, 1] <== [2, 2]
