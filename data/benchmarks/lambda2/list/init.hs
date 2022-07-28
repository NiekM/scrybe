{-# INCLUDE foldList, elimList, elimMaybe #-}
-- {-# INCLUDE reverse, elimList #-}

init :: List a -> Maybe (List a)
init = _

assert init [] <== Nothing
assert init [A] <== Just []
assert init [A,B] <== Just [A]
assert init [A,B,C] <== Just [A,B]
