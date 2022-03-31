takeWhile :: (A -> Bool) -> List A -> List A
takeWhile p xs = foldr (\x r -> elimBool nil (cons x r) (p x)) nil xs
