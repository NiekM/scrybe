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

-- || Booleans

data Bool = False | True

-- TODO: false, true, elimBool and similar functions could be inlined to help
-- visualization for students
false :: Bool
false = False

true :: Bool
true = True

elimBool :: a -> a -> Bool -> a
elimBool f t b = case b of
  False -> f
  True -> t

not :: Bool -> Bool
not b = elimBool True False b

-- || Orderings

data Ord = LT | EQ | GT

lt :: Ord
lt = LT

eq :: Ord
eq = EQ

gt :: Ord
gt = GT

elimOrd :: a -> a -> a -> Ord -> a
elimOrd l e g o = case o of
  LT -> l
  EQ -> e
  GT -> g

-- || Maybe

data Maybe a = Nothing | Just a

nothing :: Maybe a
nothing = Nothing

just :: a -> Maybe a
just = Just

elimMaybe :: b -> (a -> b) -> Maybe a -> b
elimMaybe n j m = case m of
  Nothing -> n
  Just x -> j x

-- || Naturals

data Nat = Zero | Succ Nat

zero :: Nat
zero = Zero

succ :: Nat -> Nat
succ = Succ

elimNat :: a -> (Nat -> a) -> Nat -> a
elimNat z s n = case n of
  Zero -> z
  Succ m -> s m

-- TODO: automatically introduce fixpoint for global bindings
foldNat :: a -> (a -> a) -> Nat -> a
foldNat z s = fix \go n -> case n of
  Zero -> z
  Succ m -> s (go m)

unfoldNat :: (a -> Maybe a) -> a -> Nat
unfoldNat f x = case f x of
  Nothing -> Zero
  Just y -> Succ (unfoldNat f y)

plus :: Nat -> Nat -> Nat
plus n = foldNat n Succ

mult :: Nat -> Nat -> Nat
mult n = foldNat Zero (plus n)

compareNat :: Nat -> Nat -> Ord
compareNat = foldNat (elimNat EQ (const LT)) \n -> elimNat GT \m -> n m

-- || Lists

data List a = Nil | Cons a (List a)

nil :: List a
nil = Nil

cons :: a -> List a -> List a
cons = Cons

elimList :: a -> (b -> List b -> a) -> List b -> a
elimList n c l = case l of
  Nil -> n
  Cons h t -> c h t

foldList :: b -> (a -> b -> b) -> List a -> b
foldList n c = fix \go l -> case l of
  Nil -> n
  Cons h t -> c h (go t)

foldr :: (a -> b -> b) -> b -> List a -> b
foldr f e = foldList e f

paraList :: b -> (a -> List a -> b -> b) -> List a -> b
paraList n c = fix \go l -> case l of
  Nil -> n
  Cons h t -> c h t (go t)

map :: (a -> b) -> List a -> List b
map f = foldList [] (\x -> Cons (f x))

append :: List a -> List a -> List a
append xs ys = foldList ys Cons xs

length :: List a -> Nat
length xs = foldList Zero (\x r -> Succ r) xs

sum :: List Nat -> Nat
sum = foldList 0 plus

product :: List Nat -> Nat
product = foldList 1 mult

insert :: Nat -> List Nat -> List Nat
insert n = paraList [n] \x xs r -> case compareNat n x of
  LT -> Cons n (Cons x xs)
  EQ -> Cons n (Cons x xs)
  GT -> Cons x r

sort :: List Nat -> List Nat
sort = foldList [] insert

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

zip :: List a -> List b -> List (Pair a b)
zip = foldList (const []) \x r -> elimList [] \y ys -> Cons (Pair x y) (r ys)

zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith f xs ys = map (uncurry f) (zip xs ys)

-- || Coproducts

data Either a b = Left a | Right b

left :: a -> Either a b
left = Left

right :: b -> Either a b
right = Right

elimEither :: (a -> c) -> (b -> c) -> Either a b -> c
elimEither l r e = case e of
  Left x -> l x
  Right y -> r y
