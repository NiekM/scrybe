{-# INCLUDE foldList #-}

-- TODO: what is the type of swap, what is even the model solution?
swap :: List a -> Maybe (List a)
swap = _

assert swap [] <== Just []
assert swap [A] <== Nothing
assert swap [B] <== Nothing
assert swap [A, B] <== Just [B, A]
assert swap [B, A] <== Just [A, B]
assert swap [B, A, B] <== Nothing
assert swap [A, B, A, B] <== Just [B, A, B, A]
