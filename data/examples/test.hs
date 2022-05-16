import Prelude ()

foo :: Nat
foo = 3

bar :: Bool -> Nat
bar x = {}

assert bar \False -> 5
assert bar \True  -> 6
