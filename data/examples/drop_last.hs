import Prelude (foldList)

drop_last :: List a -> List a
drop_last = _

assert drop_last []        <== []
assert drop_last [1]       <== []
assert drop_last [2]       <== []
assert drop_last [2, 1]    <== [2]
assert drop_last [1, 2]    <== [1]
assert drop_last [3, 2, 1] <== [3, 2]
