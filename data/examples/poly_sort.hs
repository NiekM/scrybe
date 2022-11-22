-- {-# INCLUDE foldr, insert #-}
{-# INCLUDE elimBool #-}
-- {-# INCLUDE paraList #-}
{-# INCLUDE paraList :: List b -> (a -> List a -> List b -> List b) -> List a -> List b #-}

sort :: (a -> a -> Bool) -> List a -> List a
sort p xs = foldr (\x r -> _) _ xs

assert sort leq []      <== []
assert sort leq [1]     <== [1]
assert sort leq [2,1]   <== [1,2]
-- assert sort leq [0,0]   <== [0,0]
assert sort leq [1,0,0] <== [0,0,1]
assert sort leq [2,3,1] <== [1,2,3]
