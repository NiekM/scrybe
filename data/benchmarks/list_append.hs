import Prelude (foldList)

append :: List a -> List a -> List a
append = {}

assert append \[]     []  -> []
assert append \[]     [0] -> [0]
assert append \[0]    []  -> [0]
assert append \[0]    [0] -> [0, 0]
assert append \[1, 0] []  -> [1, 0]
assert append \[1, 0] [0] -> [1, 0, 0]
