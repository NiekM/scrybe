-- import Prelude (foldList, elimBool)
-- import Prelude (elimBool)

-- NOTE: almost any hole introduction makes this diverge
filter :: (a -> Bool) -> List a -> List a
filter p xs = foldList {} (\x r -> elimBool {} (Cons x r) (p x)) xs

assert filter even []        <== []
assert filter even [0]       <== [0]
assert filter even [1]       <== []
-- assert filter even [2]       <== [2]
assert filter even [0, 0]    <== [0, 0]
-- assert filter even [0, 1]    <== [0]
assert filter even [1, 0, 1] <== [0]
