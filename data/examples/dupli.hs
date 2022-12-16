{-# INCLUDE foldr #-}
-- {-# INCLUDE foldl, append #-}
-- {-# INCLUDE concatMap #-}
-- {-# INCLUDE concat, map #-}
-- {-# INCLUDE interleave #-}
-- {-# INCLUDE paraList #-}
-- {-# INCLUDE multi #-}

dupli :: List a -> List a
dupli = _

assert dupli []     <== []
assert dupli [0]    <== [0, 0]
assert dupli [1, 0] <== [1, 1, 0, 0]
