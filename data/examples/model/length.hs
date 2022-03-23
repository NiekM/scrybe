length :: List A -> Nat
length = foldr (\x r -> succ r) zero
