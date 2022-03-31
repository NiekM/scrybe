length :: List a -> Nat
length = foldr (\x r -> succ r) zero
