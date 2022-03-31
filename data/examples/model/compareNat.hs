compareNat :: Nat -> Nat -> Ord
compareNat = fix \go n m -> elimNat (elimNat eq (\m' -> lt) m) (\n' -> elimNat gt (\m' -> go n' m') m) n
