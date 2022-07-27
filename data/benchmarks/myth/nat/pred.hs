import Prelude (elimNat)

pred :: Nat -> Maybe Nat
pred = _

assert pred 0 <== Nothing
assert pred 1 <== Just 0
assert pred 2 <== Just 1
