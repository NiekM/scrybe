import Prelude (foldList)

map :: (a -> b) -> List a -> List b
map = {}

assert map Succ []     <== []
-- assert map Succ [0]    <== [1]
-- assert map Succ [0, 0] <== [1, 1]
assert map Succ [1]    <== [2]
assert map Succ [1, 1] <== [2, 2]

-- TODO: if we use this function instead of the example above, the type
-- variables of map are instantiated to Nat and map specialized to Succ becomes
-- a correct refinement of map. To fix this, make sure that this type
-- unification does not carry over to the type of map in the hole context.
-- map_succ :: List Nat -> List Nat
-- map_succ = map Succ

-- NOTE: these extra examples from Smyth are not needed due to polymorphism,
-- but also make the synthesis a little slower.

-- assert map (const Unit) []     <== []
assert map (const Unit) [0]    <== [Unit]
assert map (const Unit) [0, 0] <== [Unit, Unit]
