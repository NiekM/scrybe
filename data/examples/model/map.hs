map :: (a -> b) -> List a -> List b
map f = foldr (\x r -> cons (f x) r) nil
