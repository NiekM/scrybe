-- {-# INCLUDE foldr, elimList, elimMaybe #-}
-- {-# INCLUDE reverse, elimList #-}
-- {-# INCLUDE map, filter, foldl, foldr, elimList #-}
{-# INCLUDE map, filter, foldr, elimList #-}
{-# INCLUDE elimMaybe #-}

{-# DESC "All but the last element of a list" #-}
init :: List a -> Maybe (List a)
init = _

assert init []      <== Nothing
assert init [A]     <== Just []
assert init [A,B]   <== Just [A]
assert init [A,B,C] <== Just [A,B]
