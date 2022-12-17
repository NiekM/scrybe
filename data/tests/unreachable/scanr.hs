{-# INCLUDE foldr #-}

-- NOTE: Implementations of scanr in terms of foldr make use of the fact that
-- the recursive argument `r` in `foldr (\x r -> _) [e] xs` is never empty,
-- since it will be passed `[e]` in case `xs` is empty and the list only grows.
-- As such, the case where `r` is empty has no constraints over it, leading to
-- dead code. One natural way to deal with dead code is to finish synthesis not
-- when no holes are left, but when no constraints are left. The leftover holes
-- represent unreachable code.
scanr :: (a -> b -> b) -> b -> List a -> List b
scanr = _

assert scanr plus 0 []        <== [0]
assert scanr plus 0 [1]       <== [1,0]
assert scanr plus 0 [2, 1]    <== [3,1,0]
assert scanr plus 0 [3, 2, 1] <== [6,3,1,0]
assert scanr plus 1 []        <== [1]
