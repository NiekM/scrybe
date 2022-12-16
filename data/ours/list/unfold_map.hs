{-# INCLUDE unfoldr, elimList #-}

-- NOTE: here evaluation order seems to go wrong
map :: (a -> b) -> List a -> List b
map f xs = _
-- map f xs = unfoldr _ _
-- map f = unfoldr _

assert map Succ []     <== []
assert map Succ [1]    <== [2]
assert map Succ [1, 1] <== [2, 2]
assert map Succ [0, 0, 0, 0] <== [1, 1, 1, 1]

assert map (const Unit) [0]    <== [Unit]
assert map (const Unit) [0, 0] <== [Unit, Unit]
