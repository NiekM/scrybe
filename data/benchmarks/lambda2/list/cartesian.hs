-- Taken from the lambda^2 benchmark
import Prelude (foldList)

-- Model solution:
-- cartesian xss = foldList [[]] (\xs yss -> foldList [] (\x zss -> foldList zss (\ys qss -> Cons (Cons x ys) qss) yss) xs) xss

cartesian :: List (List a) -> List (List a)
cartesian = {}

assert cartesian []              <== [[]]
assert cartesian [[]]            <== []
assert cartesian [[],[]]         <== []
assert cartesian [[A,B,C],[D,E]] <== [[A,D],[A,E],[B,D],[B,E],[C,D],[C,E]]
