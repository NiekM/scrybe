{-# INCLUDE unfoldList, elimList #-}

map :: (a -> b) -> List a -> List b
map = _

assert map Succ []     <== []
-- assert map Succ [0]    <== [1]
-- assert map Succ [0, 0] <== [1, 1]
assert map Succ [1]    <== [2]
assert map Succ [1, 1] <== [2, 2]

-- NOTE: these extra examples from Smyth are not needed due to polymorphism,
-- but also make the synthesis a little slower.

-- assert map (const Unit) []     <== []
assert map (const Unit) [0]    <== [Unit]
assert map (const Unit) [0, 0] <== [Unit, Unit]
