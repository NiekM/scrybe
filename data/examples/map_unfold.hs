{-# INCLUDE unfoldr, elimList #-}

-- TODO: unfoldr seems to not work without the second argument given, perhaps
-- because it is never in a blocking position.
map :: (a -> b) -> List a -> List b
map f xs = unfoldr _ xs

assert map succ []     <== []
assert map succ [0]    <== [1]
assert map succ [0, 0] <== [1, 1]
assert map succ [1]    <== [2]
assert map succ [1, 1] <== [2, 2]
