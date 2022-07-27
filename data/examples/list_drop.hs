import Prelude (elimList)

-- MODEL:
-- drop n xs = foldNat (\ys -> ys) (\r ys -> elimList [] (\z zs -> r zs) ys) n xs

-- NOTE: like with filter, almost any hole introduced makes it diverge
drop :: Nat -> List a -> List a
drop n xs = foldNat _ (\r -> elimList [] (\z zs -> r zs)) n xs

assert drop 0 []                 <== []
assert drop 0 [A]                <== [A]
assert drop 1 []                 <== []
assert drop 1 [A, B, C]          <== [B, C]
assert drop 2 [A]                <== []
assert drop 2 [A, B, C]          <== [C]
assert drop 4 [A, B, C, D, E, F] <== [E, F]
