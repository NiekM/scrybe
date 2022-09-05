{-# INCLUDE foldList #-}

foldr :: (a -> b -> b) -> b -> List a -> b
foldr = _

assert foldr plus 0 []        <== 0
assert foldr plus 0 [1]       <== 1
assert foldr plus 0 [2, 1]    <== 3
assert foldr plus 0 [3, 2, 1] <== 6
assert foldr plus 1 []        <== 1
assert foldr or False []                   <== False
assert foldr or True  []                   <== True
assert foldr or False [True, False]        <== True
assert foldr or False [False, False]       <== False
assert foldr or False [False, False, True] <== True
