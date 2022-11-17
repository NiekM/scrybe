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

{-# FORBID elimBool _ _ False #-}
{-# FORBID elimBool _ _ True #-}
{-# FORBID elimBool False True _ #-}

elimBool :: a -> a -> Bool -> a
elimBool f t b = case b of
  False -> f
  True -> t

{-# FORBID not True #-}
{-# FORBID not False #-}

not :: Bool -> Bool
not b = elimBool True False b

{-# FORBID and True _ #-}
{-# FORBID and _ True #-}
{-# FORBID and False _ #-}
{-# FORBID and _ False #-}
{-# FORBID and (and _ _) _ #-}

and :: Bool -> Bool -> Bool
and = elimBool False

{-# FORBID or True _ #-}
{-# FORBID or _ True #-}
{-# FORBID or False _ #-}
{-# FORBID or _ False #-}
{-# FORBID or (or _ _) _ #-}

or :: Bool -> Bool -> Bool
or x y = elimBool y True x

-- || Orderings

data Ord = LT | EQ | GT

{-# FORBID elimOrd _ _ _ LT #-}
{-# FORBID elimOrd _ _ _ EQ #-}
{-# FORBID elimOrd _ _ _ GT #-}

elimOrd :: a -> a -> a -> Ord -> a
elimOrd l e g o = case o of
  LT -> l
  EQ -> e
  GT -> g

-- || Maybe

data Maybe a = Nothing | Just a

{-# FORBID elimMaybe _ _ Nothing #-}
{-# FORBID elimMaybe _ _ (Just _) #-}
{-# FORBID elimMaybe Nothing (\x -> Just _) _ #-}

elimMaybe :: b -> (a -> b) -> Maybe a -> b
elimMaybe n j m = case m of
  Nothing -> n
  Just x -> j x

-- || Naturals

data Nat = Zero | Succ Nat

{-# FORBID elimNat _ _ 0 #-}
{-# FORBID elimNat _ _ (Succ _) #-}

elimNat :: a -> (Nat -> a) -> Nat -> a
elimNat z s n = case n of
  Zero -> z
  Succ m -> s m

{-# FORBID foldrNat _ _ 0 #-}
{-# FORBID foldrNat _ _ (Succ _) #-}

foldrNat :: a -> (a -> a) -> Nat -> a
foldrNat z s n = case n of
  Zero -> z
  Succ m -> s (foldrNat z s m)

foldlNat :: (a -> a) -> a -> Nat -> a
foldlNat s acc n = case n of
  Zero -> acc
  Succ m -> foldlNat s (s acc) m

-- paraNat :: a -> (Nat -> a -> a) -> Nat -> a
-- paraNat z s n = case n of
--   Zero -> z
--   Succ m -> s m (paraNat z s m)

-- unfoldNat :: (a -> Maybe a) -> a -> Nat
-- unfoldNat f x = case f x of
--   Nothing -> Zero
--   Just y -> Succ (unfoldNat f y)

{-# FORBID even 0 #-}
{-# FORBID even (Succ _) #-}

even :: Nat -> Bool
even = foldlNat not True

{-# FORBID odd 0 #-}
{-# FORBID odd (Succ _) #-}

odd :: Nat -> Bool
odd = foldlNat not False

{-# FORBID plus _ 0 #-}
{-# FORBID plus 0 _ #-}
{-# FORBID plus (plus _ _) _ #-}
{-# FORBID plus (Succ _) _ #-}
{-# FORBID plus _ (Succ _) #-}

plus :: Nat -> Nat -> Nat
plus n = foldrNat n Succ

{-# FORBID mult _ 1 #-}
{-# FORBID mult 1 _ #-}
{-# FORBID mult (mult _ _) _ #-}

mult :: Nat -> Nat -> Nat
mult n = foldrNat Zero (plus n)

{-# FORBID compareNat 0 0 #-}
{-# FORBID compareNat 0 (Succ _) #-}
{-# FORBID compareNat (Succ _) 0 #-}
{-# FORBID compareNat (Succ _) (Succ _) #-}
{-# FORBID compareNat Zero _ #-}
{-# FORBID compareNat (Succ _) _ #-}

compareNat :: Nat -> Nat -> Ord
compareNat = foldrNat (elimNat EQ (const LT)) \n -> elimNat GT \m -> n m

{-# FORBID eq 0 0 #-}
{-# FORBID eq 0 (Succ _) #-}
{-# FORBID eq (Succ _) 0 #-}
{-# FORBID eq (Succ _) (Succ _) #-}
{-# FORBID eq Zero _ #-}
{-# FORBID eq (Succ _) _ #-}

eq :: Nat -> Nat -> Bool
eq n m = elimOrd False True False (compareNat n m)

{-# FORBID neq 0 0 #-}
{-# FORBID neq 0 (Succ _) #-}
{-# FORBID neq (Succ _) 0 #-}
{-# FORBID neq (Succ _) (Succ _) #-}
{-# FORBID neq Zero _ #-}
{-# FORBID neq (Succ _) _ #-}

neq :: Nat -> Nat -> Bool
neq n m = not (eq n m)

{-# FORBID leq 0 _ #-}
{-# FORBID leq (Succ _) (Succ _) #-}

leq :: Nat -> Nat -> Bool
leq n m = elimOrd True True False (compareNat n m)

{-# FORBID max 0 _ #-}
{-# FORBID max _ 0 #-}
{-# FORBID max (max _ _) _ #-}
{-# FORBID max (Succ _) (Succ _) #-}
{-# FORBID max (Succ _) _ #-}

max :: Nat -> Nat -> Nat
max n m = elimBool n m (leq n m)

{-# FORBID double 0 #-}
{-# FORBID double (Succ _) #-}

double :: Nat -> Nat
double n = case n of
  Zero -> Zero
  Succ m -> Succ (Succ (double m))

-- || Lists

data List a = Nil | Cons a (List a)

{-# FORBID elimList _ _ [] #-}
{-# FORBID elimList _ _ (Cons _ _) #-}

elimList :: a -> (b -> List b -> a) -> List b -> a
elimList n c l = case l of
  Nil -> n
  Cons h t -> c h t

{-# FORBID head Nil #-}
{-# FORBID head (Cons _ _) #-}

head :: List a -> Maybe a
head = elimList Nothing \x xs -> Just x

{-# FORBID tail Nil #-}
{-# FORBID tail (Cons _ _) #-}

tail :: List a -> Maybe (List a)
tail = elimList Nothing \x xs -> Just xs

{-# FORBID foldr _ _ Nil #-}
{-# FORBID foldr _ _ (Cons _ _) #-}
{-# FORBID foldr (\x r -> r) _ _ #-}
{-# FORBID foldr (\x r -> Nil) _ _ #-}
{-# FORBID foldr (\x r -> Cons x r) Nil _ #-}

foldr :: (a -> b -> b) -> b -> List a -> b
foldr f e = foldList e f

foldList :: b -> (a -> b -> b) -> List a -> b
foldList n c l = case l of
  Nil -> n
  Cons h t -> c h (foldList n c t)

{-# FORBID paraList _ _ Nil #-}
{-# FORBID paraList _ _ (Cons _ _) #-}

paraList :: b -> (a -> List a -> b -> b) -> List a -> b
paraList n c l = case l of
  Nil -> n
  Cons h t -> c h t (paraList n c t)

mapList :: (a -> b) -> List a -> List b
mapList f = foldList [] (\x -> Cons (f x))

{-# FORBID foldl _ _ Nil #-}
{-# FORBID foldl _ _ (Cons _ _) #-}

foldl :: (b -> a -> b) -> b -> List a -> b
foldl f acc l = case l of
  Nil -> acc
  Cons h t -> foldl f (f acc h) t

{-# FORBID map _ Nil #-}
{-# FORBID map _ (Cons _ _) #-}
{-# FORBID map (\x -> x) _ #-}
{-# FORBID map _ (map _ _) #-}

map :: (a -> b) -> List a -> List b
map f = foldr (\x -> Cons (f x)) []

filter :: (a -> Bool) -> List a -> List a
filter p = foldList [] (\x r -> elimBool r (Cons x r) (p x))

{-# FORBID append Nil _ #-}
{-# FORBID append _ Nil #-}
{-# FORBID append (append _ _) _ #-}
{-# FORBID append (Cons _ _) _ #-}

append :: List a -> List a -> List a
append xs ys = foldList ys Cons xs

{-# FORBID snoc Nil _ #-}
{-# FORBID snoc (Cons _ _) _ #-}

snoc :: List a -> a -> List a
snoc xs x = foldList [x] Cons xs

{-# FORBID reverse (reverse _) #-}

reverse :: List a -> List a
reverse = foldList [] (flip snoc)

{-# FORBID concat Nil #-}
{-# FORBID concat (Cons _ _) #-}

concat :: List (List a) -> List a
concat = foldList [] append

{-# FORBID concatMap _ Nil #-}
{-# FORBID concatMap _ (Cons _ _) #-}
{-# FORBID concatMap (\x -> x) _ #-}
{-# FORBID concatMap (\x -> []) _ #-}

concatMap :: (a -> List b) -> List a -> List b
concatMap f xs = concat (map f xs)

catMaybes :: List (Maybe a) -> List a
catMaybes = foldList [] \x r -> case x of
  Nothing -> r
  Just y  -> Cons y r

mapMaybe :: (a -> Maybe b) -> List a -> List b
mapMaybe f xs = catMaybes (map f xs)

{-# FORBID length Nil #-}
{-# FORBID length (Cons _ _) #-}

length :: List a -> Nat
length = foldl (\r x -> Succ r) Zero

{-# FORBID sum Nil #-}
{-# FORBID sum (Cons _ _) #-}

sum :: List Nat -> Nat
sum = foldl plus 0

{-# FORBID sumrec Nil #-}
{-# FORBID sumrec (Cons _ _) #-}

sumrec :: List Nat -> Nat
sumrec xs = case xs of
  Nil -> Zero
  Cons y ys -> plus y (sumrec ys)

product :: List Nat -> Nat
product = foldl mult 1

maximum :: List Nat -> Nat
maximum = foldl max 0

{-# FORBID any _ Nil #-}
{-# FORBID any (\x -> False) _ #-}
{-# FORBID any (\x -> True) _ #-}

any :: (a -> Bool) -> List a -> Bool
any p = foldList False \x -> or (p x)

{-# FORBID elem _ Nil #-}
{-# FORBID elem _ (Cons _ _) #-}

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

-- zip :: List a -> List b -> List (Pair a b)
-- zip = foldList (const []) \x r -> elimList [] \y ys -> Cons (Pair x y) (r ys)

zip :: List a -> List b -> List (Pair a b)
zip xs ys = case xs of
  Nil -> Nil
  Cons z zs -> case ys of
    Nil -> Nil
    Cons w ws -> Cons (Pair z w) (zip zs ws)

interleave :: List a -> List a -> List a
interleave xs ys = case xs of
  Nil -> ys
  Cons z zs -> Cons z (interleave ys zs)

zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith f xs ys = mapList (uncurry f) (zip xs ys)

unfoldList :: (b -> Maybe (Pair a b)) -> b -> List a
unfoldList f x = case f x of
  Nothing -> Nil
  Just y -> case y of
    Pair a r -> Cons a (unfoldList f r)

-- || Coproducts

data Either a b = Left a | Right b

elimEither :: (a -> c) -> (b -> c) -> Either a b -> c
elimEither l r e = case e of
  Left x -> l x
  Right y -> r y

-- || Trees

data Tree a = Leaf | Node (Tree a) a (Tree a)

{-# FORBID elimTree _ _ Leaf #-}
{-# FORBID elimTree _ _ (Node _ _ _) #-}

elimTree :: a -> (Tree b -> b -> Tree b -> a) -> Tree b -> a
elimTree e f t = case t of
  Leaf -> e
  Node l x r -> f l x r

{-# FORBID foldTree _ _ Leaf #-}
{-# FORBID foldTree _ _ (Node _ _ _) #-}

foldTree :: b -> (b -> a -> b -> b) -> Tree a -> b
foldTree e f t = case t of
  Leaf -> e
  Node l x r -> f (foldTree e f l) x (foldTree e f r)

{-# FORBID mapTree _ Leaf #-}
{-# FORBID mapTree _ (Node _ _ _) #-}
{-# FORBID mapTree (\x -> x) _ #-}
{-# FORBID mapTree _ (mapTree _ _) #-}

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f = foldTree Leaf \l x r -> Node l (f x) r

-- || Other

data Money = One | Two | Check Nat

elimMoney :: a -> a -> (Nat -> a) -> Money -> a
elimMoney one two check m = case m of
  One -> one
  Two -> two
  Check n -> check n
