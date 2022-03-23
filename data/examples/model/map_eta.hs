map :: (A -> B) -> List A -> List B
map f = foldr (\x r -> Cons (f x) r) Nil
