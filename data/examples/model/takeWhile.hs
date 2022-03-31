takeWhile :: (a -> Bool) -> List a -> List a
takeWhile p xs = foldr (\x r -> elimBool nil (cons x r) (p x)) nil xs
