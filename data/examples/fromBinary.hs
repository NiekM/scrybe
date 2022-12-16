{-# INCLUDE foldl, plus, elimBool #-}

fromBinary :: List Bool -> Nat
fromBinary = _

assert fromBinary [] <== 0
assert fromBinary [True] <== 1
assert fromBinary [True,False] <== 2
assert fromBinary [True,True] <== 3
assert fromBinary [True,False,False] <== 4
