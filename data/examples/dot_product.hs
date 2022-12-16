-- {-# INCLUDE sum, zipWith, mult #-}
{-# INCLUDE foldr, zipWith, mult, foldrNat #-}

dot :: List Nat -> List Nat -> Nat
dot xs ys = _

-- NOTE: dot product is too difficult because sum cannot be efficiently
-- unevaluated and we don't know the intermediate result...

assert dot [] [] <== 0
assert dot [2] [1] <== 2
assert dot [0] [2] <== 0
assert dot [1,0] [2] <== 2
assert dot [1,1] [2,1] <== 3
