import Prelude (foldList, plus)
-- import Prelude (foldList, foldNat)

-- NOTE: unlike Smyth, it is actually faster to just introduce foldNat i.o.
-- plus, probably because plus is not weighted correctly.
sum :: List Nat -> Nat
sum = _

assert sum []     <== 0
assert sum [1]    <== 1
assert sum [2, 1] <== 3
assert sum [1, 3] <== 4
