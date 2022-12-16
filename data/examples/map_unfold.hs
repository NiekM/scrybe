{-# INCLUDE unfoldr, elimList #-}

-- TODO: unfoldr seems to not work without the second argument given, perhaps
-- because it is never in a blocking position.
map :: (a -> b) -> List a -> List b
map f xs = unfoldr _ xs

assert map Succ []     <== []
assert map Succ [0]    <== [1]
assert map Succ [0, 0] <== [1, 1]
assert map Succ [1]    <== [2]
assert map Succ [1, 1] <== [2, 2]
