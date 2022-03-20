id :: forall a. a -> a
id x = x

const :: forall a b. a -> b -> a
const x y = x

fix :: forall a. (a -> a) -> a
fix = let go = \f -> f (go f) in go

flip :: forall a b c. (a -> b -> c) -> b -> a -> c
flip f x y = f y x

compose :: forall a b c. (b -> c) -> (a -> b) -> (a -> c)
compose f g x = f (g x)

rec :: forall a b. ((a -> b) -> (a -> b)) -> a -> b
rec = fix

data Bool = False | True

elimBool :: forall a. a -> a -> Bool -> a
elimBool f t b = case b of False -> f; True -> t

not :: Bool -> Bool
not = elimBool True False

data Ord = LT | EQ | GT

elimOrd :: forall a. a -> a -> a -> Ord -> a
elimOrd l e g o = case o of LT -> l; EQ -> e; GT -> g

data Nat = Zero | Succ Nat

elimNat :: forall a. a -> (Nat -> a) -> Nat -> a
elimNat z s n = case n of Zero -> z; Succ m -> s m

foldNat :: forall a. a -> (a -> a) -> Nat -> a
foldNat z s = let go = \n -> case n of Zero -> z; Succ m -> s (go m) in go

plus :: Nat -> Nat -> Nat
plus n = foldNat n Succ

mult :: Nat -> Nat -> Nat
mult = Mult -- TODO: make sure this works correctly

data List a = Nil | Cons a (List a)

nil :: forall a. List a
nil = Nil

cons :: forall a. a -> List a -> List a
cons = Cons

elimList :: forall a b. a -> (b -> List b -> a) -> List b -> a
elimList n c l = case l of Nil -> n; Cons h t -> c h t

foldList :: forall a b. b -> (a -> b -> b) -> List a -> b
foldList n c = let go = \l -> case l of Nil -> n; Cons h t -> c h (go t) in go

foldr :: forall a b. (a -> b -> b) -> b -> List a -> b
foldr = flip foldList

map :: forall a b. (a -> b) -> List a -> List b
map f = foldList [] (\x -> Cons (f x))

data Pair a b = Pair a b

fst :: forall a b. Pair a b -> a
fst p = case p of Pair x y -> x

snd :: forall a b. Pair a b -> b
snd p = case p of Pair x y -> y

swap :: forall a b. Pair a b -> Pair b a
swap p = case p of Pair x y -> Pair y x

curry :: forall a b c. (Pair a b -> c) -> a -> b -> c
curry f x y = f (Pair x y)

uncurry :: forall a b c. (a -> b -> c) -> Pair a b -> c
uncurry f p = case p of Pair x y -> f x y

data Either a b = Left a | Right b

elimEither :: forall a b c. (a -> c) -> (b -> c) -> Either a b -> c
elimEither l r e = case e of Left x -> l x; Right y -> r y

