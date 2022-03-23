length :: List A -> Nat
length = foldr (\x r -> Succ r) Zero
