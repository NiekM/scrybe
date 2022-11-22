{-# INCLUDE foldr #-}
-- {-# INCLUDE map, filter, foldl, foldr, elimList #-}
-- {-# INCLUDE map, filter, foldl, foldr #-}
-- {-# INCLUDE filter, foldl, foldr #-}

{-# DESC "The cartesian product" #-}
cartesian :: List (List a) -> List (List a)
cartesian = _

assert cartesian []              <== [[]]
assert cartesian [[]]            <== []
assert cartesian [[],[]]         <== []
assert cartesian [[A,B,C],[D,E]] <== [[A,D],[A,E],[B,D],[B,E],[C,D],[C,E]]
