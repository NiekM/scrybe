import Prelude (foldList)

concat :: List (List a) -> List a
concat = {}

assert concat []    <== []
assert concat [[]]  <== []
assert concat [[0]] <== [0]
assert concat [[0], [1]] <== [0, 1]
