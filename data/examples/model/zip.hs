zip :: List a -> List b -> List (Pair a b)
zip = foldList (\r -> nil) \x f -> elimList nil \y ys -> cons (pair x y) (f ys)
