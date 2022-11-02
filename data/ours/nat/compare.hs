-- {-# INCLUDE foldNatIndexed, elimNat #-}
{-# INCLUDE foldrNat :: (Nat -> b) -> ((Nat -> b) -> Nat -> b) -> Nat -> Nat -> b #-}
{-# INCLUDE elimNat #-}

-- NOTE: it works without sketch, but is very slow!
compare :: Nat -> Nat -> Ord
-- compare = foldNat _ _
compare = _

assert compare 0 0 <== EQ
assert compare 0 1 <== LT
assert compare 1 0 <== GT
assert compare 1 1 <== EQ
assert compare 1 3 <== LT
assert compare 3 2 <== GT