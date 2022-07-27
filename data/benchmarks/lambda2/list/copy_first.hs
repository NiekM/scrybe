import Prelude (foldList, elimList)

copy_first :: List a -> List a
copy_first = {}

assert copy_first [] <== []
assert copy_first [A] <== [A]
assert copy_first [A,B] <== [A,A]
assert copy_first [A,B,C] <== [A,A,A]
