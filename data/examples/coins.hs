-- (  1.41 secs,    470,926,736 bytes)
{-# INCLUDE sum, map, elimMoney #-}

-- (112.79 secs, 49,066,661,624 bytes)
-- {-# INCLUDE sum, map, elimMoney, foldr #-}

-- TODO: figure out why it is so much slower with foldr.
-- Perhaps the problem is that, despite constructors not having a large weight,
-- the amount of intermediate states they introduce where folds could be used
-- is very large.


coins :: List Money -> Nat
coins xs = _

assert coins [] <== 0
assert coins [One] <== 1
assert coins [One, One] <== 2
assert coins [Two] <== 2
assert coins [Check 0, Two] <== 2
assert coins [One, Check 1] <== 2
assert coins [One, Two] <== 3
assert coins [Check 3] <== 3
assert coins [Two, Two] <== 4
