-- import Prelude (foldList, snoc)
import Prelude (foldList)

-- TODO: what is the recursion scheme?

rev :: List a -> List a
rev = _

assert rev []        <== []
assert rev [0]       <== [0]
assert rev [1]       <== [1]
assert rev [0, 1]    <== [1, 0]
assert rev [0, 0, 1] <== [1, 0, 0]
