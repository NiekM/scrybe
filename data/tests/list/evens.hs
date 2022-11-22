{-# INCLUDE map, filter, foldl, foldr, elimList #-}
{-# INCLUDE elimBool, even #-}

{-# DESC "Remove any odd numbers from a list" #-}
evens :: List Nat -> List Nat
evens = _

assert evens []        <== []
assert evens [0]       <== [0]
assert evens [1]       <== []
-- assert evens [2]       <== [2]
assert evens [0, 0]    <== [0, 0]
-- assert evens [0, 1]    <== [0]
assert evens [1, 0, 1] <== [0]
