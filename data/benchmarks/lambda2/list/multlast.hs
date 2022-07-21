import Prelude (foldList, elimList)

multlast :: List a -> List a
multlast = {}

assert multlast [] <== []
assert multlast [A] <== [A]
assert multlast [A,B] <== [B,B]
assert multlast [A,B,C] <== [C,C,C]
