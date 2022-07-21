import Prelude (foldList, elimList)

shiftr :: List a -> List a
shiftr = {}

assert shiftr [] <== []
assert shiftr [A] <== [A]
assert shiftr [A,B] <== [B,A]
assert shiftr [A,B,C] <== [C,A,B]
