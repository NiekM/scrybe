import Prelude

compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g x = {}

assert compose not not True  <== True
assert compose not not False <== False
