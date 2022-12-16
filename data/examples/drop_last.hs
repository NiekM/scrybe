{-# INCLUDE foldr, elimList #-}

drop_last :: List a -> List a
drop_last = _

assert drop_last []        <== []
assert drop_last [A]       <== []
assert drop_last [A,B]     <== [A]
assert drop_last [A,B,C]   <== [A,B]
assert drop_last [A,B,C,D] <== [A,B,C]
