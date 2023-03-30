{-# INCLUDE foldr, elimList #-}

{-# DESC "A catamorphism over a list" #-}
fold :: (a -> b -> b) -> b -> List a -> b
fold = _

assert fold plus 0 []        <== 0
assert fold plus 0 [1]       <== 1
assert fold plus 0 [2, 1]    <== 3
assert fold plus 0 [3, 2, 1] <== 6
assert fold plus 1 []        <== 1
assert fold or False []                   <== False
assert fold or True  []                   <== True
assert fold or False [True, False]        <== True
assert fold or False [False, False]       <== False
assert fold or False [False, False, True] <== True
