nil :: List B
cons :: B -> List B -> List B
foldr :: (A -> List B -> List B) -> List B -> List A -> List B
compose :: (B -> List B -> List B) -> (A -> B) -> A -> List B -> List B
