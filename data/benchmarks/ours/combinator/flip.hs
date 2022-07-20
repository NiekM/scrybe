import Prelude

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = {}

assert flip Cons [] 1 <== [1]
