compose :: (B -> C) -> (A -> B) -> (A -> C)
compose f g x = f (g x)
