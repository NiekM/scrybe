import Prelude (foldList, elimList)

multfirst :: List a -> List a
multfirst = {}

assert multfirst [] <== []
assert multfirst [A] <== [A]
assert multfirst [A,B] <== [A,A]
assert multfirst [A,B,C] <== [A,A,A]
