{-# INCLUDE foldr, elimList #-}

tail :: List a -> List a
tail = _

assert tail [1]           <== []
assert tail [True, False] <== [False]
assert tail [0, 1, 2]     <== [1, 2]
