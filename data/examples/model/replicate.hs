replicate :: Nat -> A -> List A
replicate n x = foldNat nil (cons x) n
