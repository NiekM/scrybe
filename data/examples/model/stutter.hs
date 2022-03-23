stutter :: List A -> List A
stutter = foldr (\y r -> Cons y (Cons y r)) Nil
