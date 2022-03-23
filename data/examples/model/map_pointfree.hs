map :: (A -> B) -> List A -> List B
map f = foldr (compose Cons f) Nil
