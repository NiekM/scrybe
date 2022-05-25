import Prelude (elimList)

tail :: List a -> Maybe (List a)
tail = {}

assert tail \[]            -> Nothing
assert tail \[1]           -> Just []
assert tail \[True, False] -> Just [False]
assert tail \[0, 1, 2]     -> Just [1, 2]
