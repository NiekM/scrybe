{-# INCLUDE foldr, elimList #-}
{-# INCLUDE elimBool, eq #-}

{-# DESC "Remove consecutive duplicates from a list" #-}
compress :: List Nat -> List Nat
compress = _

assert compress [] <== []
assert compress [0] <== [0]
assert compress [1] <== [1]
assert compress [0,0] <== [0]
assert compress [1,1] <== [1]
assert compress [2,0] <== [2,0]
assert compress [1,0,0] <== [1,0]
assert compress [0,1,1] <== [0,1]
assert compress [2,1,0,0] <== [2,1,0]
assert compress [2,2,1,0,0] <== [2,1,0]
assert compress [2,2,0] <== [2,0]
assert compress [2,2,2,0] <== [2,0]
assert compress [1,2,2,2,0] <== [1,2,0]
