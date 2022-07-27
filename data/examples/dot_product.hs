-- import Prelude (sum, zipWith, mult)
-- import Prelude (sum, zipWith, foldNat)
-- import Prelude (foldList, zipWith, foldNat, plus)
-- import Prelude (sum, zipWith, foldNat, plus)
import Prelude (zipWith, foldNat, plus)
-- import Prelude (foldList, zipWith, mult, plus)
-- import Prelude (zipWith, mult)

dot :: List Nat -> List Nat -> Nat
-- dot :: List Nat -> List Nat -> List Nat
dot xs ys = foldList _ _ _

-- NOTE: dot product is too difficult because sum cannot be efficiently
-- unevaluated and we don't know the intermediate result...

assert dot [] [] <== 0
assert dot [2] [1] <== 2
assert dot [0] [2] <== 0
assert dot [1,0] [2] <== 2
assert dot [1,1] [2,1] <== 3
-- assert dot [1,2] [3,4] <== 11
-- assert dot [2,2] [3,3] <== 12
-- assert dot [] [] <== []
-- assert dot [2] [1] <== [2]
-- assert dot [0] [2] <== [0]
-- assert dot [1,2] [3,4] <== [3,8]
