{-# INCLUDE foldr #-}

insert :: Nat -> List Nat -> List Nat
insert n = paraList [n] \x xs r -> case leq n x of
  True  -> Cons n _
  False -> Cons x _

sort :: List Nat -> List Nat
sort = _

assert sort []        <== []
assert sort [1]       <== [1]
assert sort [2,1]     <== [1,2]
assert sort [0,0]     <== [0,0]
assert sort [1,0,0]   <== [0,0,1]
assert sort [1,2,0]   <== [0,1,2]
assert sort [1,0,1,0] <== [0,0,1,1]
