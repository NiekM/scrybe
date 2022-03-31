stutter :: List a -> List a
stutter = foldr (\y r -> cons y (cons y r)) nil
