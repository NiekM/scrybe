flip :: (A -> B -> C) -> B -> A -> C
flip f x y = f y x
