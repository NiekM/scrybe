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

-- || Unit

data Unit = Unit

-- || Booleans

data Bool = False | True

elimBool :: a -> a -> Bool -> a
elimBool f t b = case b of
  False -> f
  True -> t

not :: Bool -> Bool
not b = elimBool True False b

and :: Bool -> Bool -> Bool
and = elimBool False

or :: Bool -> Bool -> Bool
or x y = elimBool y True x

-- || Orderings

data Ord = LT | EQ | GT

elimOrd :: a -> a -> a -> Ord -> a
elimOrd l e g o = case o of
  LT -> l
  EQ -> e
  GT -> g

-- || Maybe

data Maybe a = Nothing | Just a

elimMaybe :: b -> (a -> b) -> Maybe a -> b
elimMaybe n j m = case m of
  Nothing -> n
  Just x -> j x

-- || Naturals

data Nat = Zero | Succ Nat

elimNat :: a -> (Nat -> a) -> Nat -> a
elimNat z s n = case n of
  Zero -> z
  Succ m -> s m

-- TODO: automatically introduce fixpoint for global bindings

foldrNat :: a -> (a -> a) -> Nat -> a
foldrNat z s = fix \go n -> case n of
  Zero -> z
  Succ m -> s (go m)

foldlNat :: (a -> a) -> a -> Nat -> a
foldlNat s = fix \go acc n -> case n of
  Zero -> acc
  Succ m -> go (s acc) m

paraNat :: a -> (Nat -> a -> a) -> Nat -> a
paraNat z s = fix \go n -> case n of
  Zero -> z
  Succ m -> s m (go m)

unfoldNat :: (a -> Maybe a) -> a -> Nat
unfoldNat f x = case f x of
  Nothing -> Zero
  Just y -> Succ (unfoldNat f y)

even :: Nat -> Bool
even = foldlNat not True

odd :: Nat -> Bool
odd = foldlNat not False

plus :: Nat -> Nat -> Nat
plus n = foldrNat n Succ

mult :: Nat -> Nat -> Nat
mult n = foldrNat Zero (plus n)

compareNat :: Nat -> Nat -> Ord
compareNat = foldrNat (elimNat EQ (const LT)) \n -> elimNat GT \m -> n m

eq :: Nat -> Nat -> Bool
eq n m = elimOrd False True False (compareNat n m)

neq :: Nat -> Nat -> Bool
neq n m = not (eq n m)

leq :: Nat -> Nat -> Bool
leq n m = elimOrd True True False (compareNat n m)

max :: Nat -> Nat -> Nat
max n m = elimBool n m (leq n m)

-- || Lists

data List a = Nil | Cons a (List a)

elimList :: a -> (b -> List b -> a) -> List b -> a
elimList n c l = case l of
  Nil -> n
  Cons h t -> c h t

head :: List a -> Maybe a
head = elimList Nothing \x xs -> Just x

tail :: List a -> Maybe (List a)
tail = elimList Nothing \x xs -> Just xs

foldList :: b -> (a -> b -> b) -> List a -> b
foldList n c = fix \go l -> case l of
  Nil -> n
  Cons h t -> c h (go t)

paraList :: b -> (a -> List a -> b -> b) -> List a -> b
paraList n c = fix \go l -> case l of
  Nil -> n
  Cons h t -> c h t (go t)

mapList :: (a -> b) -> List a -> List b
mapList f = foldList [] (\x -> Cons (f x))

foldr :: (a -> b -> b) -> b -> List a -> b
foldr f e = foldList e f

foldl :: (b -> a -> b) -> b -> List a -> b
foldl f = fix \go acc l -> case l of
  Nil -> acc
  Cons h t -> go (f acc h) t

map :: (a -> b) -> List a -> List b
map = mapList

filter :: (a -> Bool) -> List a -> List a
filter p = foldList [] (\x r -> elimBool r (Cons x r) (p x))

append :: List a -> List a -> List a
append xs ys = foldList ys Cons xs

snoc :: List a -> a -> List a
snoc xs x = foldList [x] Cons xs

reverse :: List a -> List a
reverse = foldList [] (flip snoc)

concat :: List (List a) -> List a
concat = foldList [] append

concatMap :: (a -> List b) -> List a -> List b
concatMap f xs = concat (map f xs)

catMaybes :: List (Maybe a) -> List a
catMaybes = foldList [] \x r -> case x of
  Nothing -> r
  Just y  -> Cons y r

mapMaybe :: (a -> Maybe b) -> List a -> List b
mapMaybe f xs = catMaybes (map f xs)

length :: List a -> Nat
length = foldl (\r x -> Succ r) Zero

sum :: List Nat -> Nat
sum = foldl plus 0

product :: List Nat -> Nat
product = foldl mult 1

maximum :: List Nat -> Nat
maximum = foldl max 0

any :: (a -> Bool) -> List a -> Bool
any p = foldList False \x -> or (p x)

elem :: Nat -> List Nat -> Bool
elem n = any (eq n)

lookup :: Nat -> List a -> Maybe a
lookup = foldrNat head \r -> elimList Nothing \x xs -> r xs

take :: Nat -> List a -> List a
take = foldrNat (const []) \r -> elimList [] \x xs -> Cons x (r xs)

drop :: Nat -> List a -> List a
drop = foldrNat id \r -> elimList [] \x xs -> r xs

takeWhile :: (a -> Bool) -> List a -> List a
takeWhile p = foldList [] \x r -> case p x of
  False -> []
  True -> Cons x r

dropWhile :: (a -> Bool) -> List a -> List a
dropWhile p = paraList [] \x xs r -> case p x of
  False -> Cons x xs
  True -> r

insert :: Nat -> List Nat -> List Nat
insert n = paraList [n] \x xs r -> case leq n x of
  True -> Cons n (Cons x xs)
  False -> Cons x r

sort :: List Nat -> List Nat
sort = foldList [] insert

set_insert :: Nat -> List Nat -> List Nat
set_insert n = paraList [n] \x xs r -> case compareNat n x of
  LT -> Cons n (Cons x xs)
  EQ -> Cons x xs
  GT -> Cons x r

nub :: List Nat -> List Nat
nub = foldList [] set_insert

eqList :: List Nat -> List Nat -> Bool
eqList = foldList (elimList True (\y ys -> False)) (\x r -> elimList False (\y ys -> and (eq x y) (r ys)))

-- || Products

data Pair a b = Pair a b

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
zipWith f xs ys = mapList (uncurry f) (zip xs ys)

-- || Coproducts

data Either a b = Left a | Right b

elimEither :: (a -> c) -> (b -> c) -> Either a b -> c
elimEither l r e = case e of
  Left x -> l x
  Right y -> r y

-- || Trees

data Tree a = Leaf | Node (Tree a) a (Tree a)

elimTree :: a -> (Tree b -> b -> Tree b -> a) -> Tree b -> a
elimTree e f t = case t of
  Leaf -> e
  Node l x r -> f l x r

foldTree :: b -> (b -> a -> b -> b) -> Tree a -> b
foldTree e f = fix \go t -> case t of
  Leaf -> e
  Node l x r -> f (go l) x (go r)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f = foldTree Leaf \l x r -> Node l (f x) r

-- || Other

data Money = One | Two | Check Nat

elimMoney :: a -> a -> (Nat -> a) -> Money -> a
elimMoney one two check m = case m of
  One -> one
  Two -> two
  Check n -> check n
