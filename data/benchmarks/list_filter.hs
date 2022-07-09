import Prelude (foldList, elimBool)
-- import Prelude (elimBool)

-- NOTE: having (p {}) in the sketch will diverge because p = even and
-- `even {} <== True` diverges
filter :: (a -> Bool) -> List a -> List a
-- filter p xs = foldList {} {} xs
filter = {}

assert filter even []        <== []
assert filter even [0]       <== [0]
assert filter even [1]       <== []
-- assert filter even [2]       <== [2]
assert filter even [0, 0]    <== [0, 0]
-- assert filter even [0, 1]    <== [0]
assert filter even [1, 0, 1] <== [0]
