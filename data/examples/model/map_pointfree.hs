map :: (a -> b) -> List a -> List b
map f = foldr (compose cons f) nil
