{-# INCLUDE foldr :: (a -> (List c -> b) -> (List c -> b)) -> (List c -> b) -> List a -> List c -> b #-}
{-# INCLUDE elimList #-}

zip :: List a -> List b -> List (Pair a b)
zip = foldr _ _
-- zip = _

assert zip [] [] <== []
assert zip [A] [] <== []
assert zip [] [B] <== []
assert zip [A] [B] <== [Pair A B]
assert zip [A,B,C] [D,E] <== [Pair A D, Pair B E]
assert zip [A,B] [D,E,F] <== [Pair A D, Pair B E]
