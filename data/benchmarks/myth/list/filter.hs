{-# INCLUDE foldList, elimBool #-}

filter :: (a -> Bool) -> List a -> List a
filter = _

assert filter even []        <== []
assert filter even [0]       <== [0]
assert filter even [1]       <== []
-- assert filter even [2]       <== [2]
assert filter even [0, 0]    <== [0, 0]
-- assert filter even [0, 1]    <== [0]
assert filter even [1, 0, 1] <== [0]
