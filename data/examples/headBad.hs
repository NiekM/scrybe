-- NOTE: This should not be able to synthesize as `\a0 -> headBad a0`.
-- As such, in the hole context, the type variable `a` should be frozen.

headBad :: List Bool -> Maybe Bool
headBad = elimList Nothing (\x xs -> Just x)

head :: List a -> Maybe a
head = _

assert head []            <== Nothing
assert head [1]           <== Just 1
assert head [True, False] <== Just True
assert head [0, 1, 2]     <== Just 0
