-- {-# INCLUDE elimNat #-}

{-# INCLUDE elimNat #-}

{-# DESC "The predecessor of a natural number." #-}
pred :: Nat -> Maybe Nat
pred = _

assert pred 0 <== Nothing
assert pred 1 <== Just 0
assert pred 2 <== Just 1
