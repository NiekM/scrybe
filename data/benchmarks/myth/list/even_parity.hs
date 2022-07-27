import Prelude (foldList, elimBool)

-- NOTE: this is very effective, but uses no unevaluation resumption
-- import Prelude (even, length, filter)

even_parity :: List Bool -> Bool
even_parity = _

assert even_parity [] <== True
assert even_parity [False, True] <== False
assert even_parity [False] <== True
assert even_parity [False, False] <== True
assert even_parity [True] <== False
assert even_parity [True, True, False] <== True
