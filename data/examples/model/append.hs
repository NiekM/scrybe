append :: List A -> List A -> List A
append xs ys = foldr cons ys xs
