map :: (A -> B) -> List A -> List B
map f = foldr (compose cons f) nil
