import Prelude (foldList)

stutter :: List a -> List a
stutter = {}

assert stutter []     <== []
assert stutter [0]    <== [0, 0]
assert stutter [1, 0] <== [1, 1, 0, 0]
