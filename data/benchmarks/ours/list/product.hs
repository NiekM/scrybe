{-# INCLUDE foldList, mult #-}
-- {-# INCLUDE foldList, foldNat #-}

-- NOTE: unlike Smyth, it is actually faster to just introduce foldNat i.o.
-- plus, probably because plus is not weighted correctly.
product :: List Nat -> Nat
product = _

assert product []     <== 1
assert product [1]    <== 1
assert product [2, 1] <== 2
assert product [1, 3] <== 3
assert product [2, 3] <== 6
