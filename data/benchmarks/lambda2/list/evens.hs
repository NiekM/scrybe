{-# INCLUDE filter, elimBool, even #-}

evens :: List Nat -> List Nat
evens = _

assert evens []        <== []
assert evens [0]       <== [0]
assert evens [1]       <== []
-- assert evens [2]       <== [2]
assert evens [0, 0]    <== [0, 0]
-- assert evens [0, 1]    <== [0]
assert evens [1, 0, 1] <== [0]