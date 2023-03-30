{-# INCLUDE foldr, elimList #-}
{-# INCLUDE elimBool #-}

{-# DESC "The elements in a list that satisfy `p`" #-}
filter :: (a -> Bool) -> List a -> List a
filter = _

assert filter vowel []        <== []
assert filter vowel [A]       <== [A]
assert filter vowel [B]       <== []
assert filter vowel [A, E]    <== [A, E]
assert filter vowel [B, E, D] <== [E]
