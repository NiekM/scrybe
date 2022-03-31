map :: (A -> B) -> List A -> List B
map f = foldr (\x r -> cons (f x) r) nil
