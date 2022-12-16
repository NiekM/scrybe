{-# INCLUDE foldr, elimList #-}

scanr :: (a -> b -> b) -> b -> List a -> List b
-- scanr f e = foldr (\x r -> elimList _ (\y ys -> Cons (f x y) (Cons y ys)) r) [e]
scanr f e = foldr (\x r -> elimList [] _ r) [e]

assert scanr plus 0 []        <== [0]
assert scanr plus 0 [1]       <== [1,0]
assert scanr plus 0 [2, 1]    <== [3,1,0]
assert scanr plus 0 [3, 2, 1] <== [6,3,1,0]
assert scanr plus 1 []        <== [1]
