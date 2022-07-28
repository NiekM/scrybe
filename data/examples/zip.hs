-- {-# INCLUDE foldListIndexed, elimList #-}
{-# INCLUDE elimList #-}

zip :: List a -> List b -> List (Pair a b)
-- zip = foldList (const []) \x r -> elimList [] \y ys -> Cons (Pair x y) (r ys)
zip = foldList _ \x r -> _
-- zip = _

assert zip [] [] <== []
assert zip [A] [] <== []
assert zip [] [B] <== []
assert zip [A] [B] <== [Pair A B]
assert zip [A,B,C] [D,E] <== [Pair A D, Pair B E]
assert zip [A,B] [D,E,F] <== [Pair A D, Pair B E]
