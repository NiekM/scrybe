-- | Prelude

-- || Combinators

id :: a -> a
id x = x

const :: a -> b -> a
const x y = x

apply :: (a -> b) -> a -> b
apply f x = f x

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g x = f (g x)

-- || Recursion

fix :: (a -> a) -> a
fix = let go = \f -> f (go f) in go

rec :: ((a -> b) -> (a -> b)) -> a -> b
rec = fix

-- || Booleans

data Bool = False | True

-- TODO: false, true, elimBool and similar functions could be inlined to help
-- visualization for students
false :: Bool
false = False

true :: Bool
true = True

elimBool :: a -> a -> Bool -> a
elimBool f t b = case b of False -> f; True -> t

not :: Bool -> Bool
not = elimBool True False

-- || Orderings

data Ord = LT | EQ | GT

lt :: Ord
lt = LT

eq :: Ord
eq = EQ

gt :: Ord
gt = GT

elimOrd :: a -> a -> a -> Ord -> a
elimOrd l e g o = case o of LT -> l; EQ -> e; GT -> g

-- || Naturals

data Nat = Zero | Succ Nat

zero :: Nat
zero = Zero

succ :: Nat -> Nat
succ = Succ

elimNat :: a -> (Nat -> a) -> Nat -> a
elimNat z s n = case n of Zero -> z; Succ m -> s m

foldNat :: a -> (a -> a) -> Nat -> a
foldNat z s = let go = \n -> case n of Zero -> z; Succ m -> s (go m) in go

plus :: Nat -> Nat -> Nat
plus n = foldNat n Succ

mult :: Nat -> Nat -> Nat
mult = Mult -- TODO: make sure this works correctly

-- || Lists

data List a = Nil | Cons a (List a)

nil :: List a
nil = Nil

cons :: a -> List a -> List a
cons = Cons

elimList :: a -> (b -> List b -> a) -> List b -> a
elimList n c l = case l of Nil -> n; Cons h t -> c h t

foldList :: b -> (a -> b -> b) -> List a -> b
foldList n c = let go = \l -> case l of Nil -> n; Cons h t -> c h (go t) in go

foldr :: (a -> b -> b) -> b -> List a -> b
foldr = flip foldList

map :: (a -> b) -> List a -> List b
map f = foldList [] (\x -> Cons (f x))

length :: List a -> Nat
length = foldList Zero \x r -> Succ r

-- || Products

data Pair a b = Pair a b

pair :: a -> b -> Pair a b
pair = Pair

fst :: Pair a b -> a
fst p = case p of Pair x y -> x

snd :: Pair a b -> b
snd p = case p of Pair x y -> y

swap :: Pair a b -> Pair b a
swap p = case p of Pair x y -> Pair y x

curry :: (Pair a b -> c) -> a -> b -> c
curry f x y = f (Pair x y)

uncurry :: (a -> b -> c) -> Pair a b -> c
uncurry f p = case p of Pair x y -> f x y

-- || Coproducts

data Either a b = Left a | Right b

left :: a -> Either a b
left = Left

right :: b -> Either a b
right = Right

elimEither :: (a -> c) -> (b -> c) -> Either a b -> c
elimEither l r e = case e of Left x -> l x; Right y -> r y

