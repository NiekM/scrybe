interleave :: List a -> List a -> List a
interleave = foldList (\r -> r) \x f xs -> cons x (elimList (f xs) (\y ys -> cons y (f ys)) xs)
