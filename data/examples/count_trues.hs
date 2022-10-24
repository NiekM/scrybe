-- {-# INCLUDE sum, map, elimBool #-}
{-# INCLUDE sumrec, map, elimBool #-}
-- {-# INCLUDE foldr, elimBool #-}

trues :: List Bool -> Nat
trues = _

assert trues [False] <== 0
assert trues [True, False] <== 1
assert trues [True, True] <== 2
