stutter :: List A -> List A
stutter = foldr (\y r -> cons y (cons y r)) nil
