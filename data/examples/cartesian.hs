-- ( 2.23 secs,    876,251,352 bytes)
{-# INCLUDE foldList #-}

-- ( 5.27 secs,  1,986,868,712 bytes)
-- {-# INCLUDE foldList, map #-}

-- ( 9.67 secs,  3,688,060,832 bytes)
-- {-# INCLUDE foldList, filter #-}

-- (24.10 secs,  7,966,672,344 bytes)
-- {-# INCLUDE foldList, map, filter #-}

-- (32.29 secs, 12,635,771,728 bytes)
-- {-# INCLUDE foldList, map, filter, elimList #-}

cartesian :: List (List a) -> List (List a)
cartesian = _

assert cartesian []              <== [[]]
assert cartesian [[]]            <== []
assert cartesian [[],[]]         <== []
assert cartesian [[A,B,C],[D,E]] <== [[A,D],[A,E],[B,D],[B,E],[C,D],[C,E]]
