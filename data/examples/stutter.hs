import Prelude

repl :: Nat -> a -> List a
repl = fix \repl n x -> case x of
  Zero -> _
  Succ m -> _

stutter :: Nat -> List a -> List a
stutter = fix \stutter n xs -> case xs of
  Nil -> Nil
  Cons y ys -> append (repl n y) (stutter n ys)

assert stutter \1 [1, 0] -> [1, 0]
assert stutter \2 [3]    -> [3, 3]
