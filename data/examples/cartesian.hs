import Prelude (foldList)

-- Model solution:
-- cartesian xss = foldList [[]] (\xs yss -> foldList [] (\x zss -> foldList zss (\ys qss -> Cons (Cons x ys) qss) yss) xs) xss

cartesian :: List (List a) -> List (List a)
-- cartesian xss = foldList [[]] (\xs yss -> foldList [] (\x zss -> foldList zss (\ys qss -> ons (Cons x ys) {}) yss) xs) xss
-- cartesian xss = foldList {} (\xs yss -> foldList {} (\x zss -> foldList {} (\ys qss -> {}) yss) xs) xss
cartesian = {}

assert cartesian []              <== [[]]
assert cartesian [[]]            <== []
assert cartesian [[],[]]         <== []
assert cartesian [[A,B,C],[D,E]] <== [[A,D],[A,E],[B,D],[B,E],[C,D],[C,E]]
