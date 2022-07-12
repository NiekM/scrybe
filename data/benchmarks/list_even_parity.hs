import Prelude (foldList, elimBool)
-- import Prelude (elimBool)

even_parity :: List Bool -> Bool
-- even_parity = foldList {} {}
even_parity = {}

assert even_parity [] <== True
assert even_parity [False, True] <== False
assert even_parity [False] <== True
assert even_parity [False, False] <== True
assert even_parity [True] <== False
assert even_parity [True, True, False] <== True
