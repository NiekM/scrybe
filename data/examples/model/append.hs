append :: List a -> List a -> List a
append xs ys = foldr cons ys xs
