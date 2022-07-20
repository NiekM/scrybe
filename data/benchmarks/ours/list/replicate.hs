import Prelude (foldNat)

replicate :: Nat -> a -> List a
replicate = {}

assert replicate 0 A <== []
assert replicate 1 B <== [B]
assert replicate 2 C <== [C, C]
assert replicate 3 D <== [D, D, D]
