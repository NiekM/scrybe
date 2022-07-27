import Prelude (elimList, elimBool, eq)
-- import Prelude (elimBool, eq)

compress :: List Nat -> List Nat
-- compress = foldList [] (\x -> elimList [x] (\y ys -> elimBool (Cons x (Cons y ys)) (Cons y ys) (eq x y)))
-- compress = foldList [] (\x -> elimList [x] (\y ys -> elimBool (Cons x (Cons y ys)) (Cons y ys) (eq x y)))
-- compress = {}
compress = foldList {} {}
-- compress xs = foldList [] (\x r -> elimList [x] {} r) xs

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
