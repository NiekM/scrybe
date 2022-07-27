import Prelude (foldNat, elimBool)

even :: Nat -> Bool
even = _

assert even 0 <== True
assert even 1 <== False
assert even 2 <== True
assert even 3 <== False
