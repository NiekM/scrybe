replicate :: Nat -> a -> List a
replicate n x = foldNat nil (cons x) n
