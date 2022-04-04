length :: List a -> Nat
length = foldr (const succ) zero
