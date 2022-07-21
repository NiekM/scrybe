import Prelude (foldList, elimList)

shiftl :: List a -> List a
shiftl = {}

assert shiftl [] <== []
assert shiftl [A] <== [A]
assert shiftl [A,B] <== [B,A]
assert shiftl [A,B,C] <== [B,C,A]
