{-# INCLUDE foldr #-}

{-# DESC "All but the first element of a list" #-}
tail :: List a -> List a
-- tail = foldr _ _
tail = _

assert tail []            <== []
assert tail [1]           <== []
assert tail [True, False] <== [False]
assert tail [0, 1, 2]     <== [1, 2]
