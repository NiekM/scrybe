{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Eval where

import Import
import Language.Syntax
import RIO.List (splitAt, maximumMaybe)
import qualified RIO.Map as Map
import Control.Monad.State
import Prettyprinter
import Language.Parser

class HasEnv a where
  env :: Lens' a (Map Var (Term Void))

instance HasEnv (Map Var (Term Void)) where
  env = id

eval' :: Map Var (Term Void) -> Term Void -> Maybe (Term Void)
eval' m e = evalStateT (eval e) m

-- | A simple evaluator/normalizer for expressions that leaves subexpressions
-- as if when they cannot be evaluated further.
-- TODO: add alpha renaming
eval :: (MonadFail m, MonadState s m, HasEnv s) => Term Void -> m (Term Void)
eval = \case
  Hole h -> return $ Hole h
  Var x -> do
    m <- use env
    case Map.lookup x m of
      Nothing -> fail $ "Unknown variable " <> show x
      Just e -> eval e
  Ctr c -> return $ Ctr c
  App f x -> do
    g <- eval f
    y <- eval x
    case g of
      Lam a z -> do
        modifying env $ Map.insert a y
        eval z
      _ -> return $ App g y
  Lam a x -> return $ Lam a x
  Let a x e -> do
    y <- eval x
    modifying env $ Map.insert a y
    eval e
  Case {} -> undefined
  -- Case x xs -> do
  --   y <- eval x
  --   let ys = traverseOf _1 (`match` y) <$> xs
  --   case msum ys of
  --     Nothing -> return $ Case y xs
  --     Just (m, e) -> do
  --       modifying env (m <>)
  --       eval e

match :: Pattern Void -> Term a -> Maybe (Map Var (Term a))
match p e = case p of
  Hole h -> absurd h
  Var a -> return $ Map.singleton a e
  Ctr c | Ctr d <- e, c == d -> return Map.empty
  -- Note: pattern matching does not check for duplicate variables in a pattern
  App f x | App g y <- e -> liftM2 (<>) (match f g) (match x y)
  _ -> fail "Pattern match failed"

type Address = Int
type Body = Expr 'Term Void

type Node = Expr' ('Base Address) 'Term Void

type Stack = [Address]
type Heap = Map Address Node
type Global = Map Var Address
type Funs = Map Var ([Var], Body)
type Def = (Var, [Var], Body)

data GraphState = GraphState
  { stack :: [Address]
  , heap :: Heap
  , global :: Global
  , funs :: Funs
  } deriving (Eq, Ord, Show)

step_ :: GraphState -> Either GraphState GraphState
step_ g@GraphState { stack, heap, global, funs } = case stack of
  [] -> Left g
  (a:s) -> case Map.lookup a heap of
    Nothing -> Left g
    Just x -> case x of
      App a1 a2 -> return $ GraphState (a1:a2:s) heap global funs
      Var v
        | Just (xs, e) <- Map.lookup v funs
        , n <- length xs
        , (as, s') <- splitAt n s
        , length as == n ->
          let (h, b) = inst e heap (Map.fromList (zip xs as) <> global)
          in return $ GraphState (b:s') h global funs
        | otherwise -> Left g
      _ -> Left g

alloc :: Heap -> Node -> (Heap, Address)
alloc h n = (Map.insert i n h, i) where
  i = case maximumMaybe (Map.keysSet h) of
    Nothing -> 0
    Just j -> j + 1

inst :: Body -> Heap -> Global -> (Heap, Address)
inst e h g = case e of
  Hole i -> absurd i
  App e1 e2 ->
    let (h1, a1) = inst e1 h g
        (h2, a2) = inst e2 h1 g
    in alloc h2 (App a1 a2)
  Var a -> (h, fromMaybe (error "Oh oh") $ Map.lookup a g)
  Ctr c -> alloc h (Ctr c)
  _ -> undefined

initialHeap :: [Def] -> (Heap, Global, Funs)
initialHeap = foldr (\(x, as, b) (h, g, f) ->
  let (h', g') = allocate (x, as, b) h
  in (h', g', Map.singleton x (as, b)) <> (h, g, f)) mempty

compile :: [Def] -> GraphState
compile program = GraphState { stack, heap, global, funs } where
  stack = [Map.findWithDefault (error "Oh no") "main" global]
  (heap, global, funs) = initialHeap program

fromAddress :: Heap -> Address -> Maybe (Term Void)
fromAddress h i = Map.lookup i h >>= \case
  App x y -> App <$> fromAddress h x <*> fromAddress h y
  Var v -> return $ Var v
  Ctr c -> return $ Ctr c
  _ -> Nothing

allocate :: Def -> Heap -> (Heap, Global)
allocate (name, _, _) heap =
  (heap', Map.singleton name address)
    where (heap', address) = alloc heap (Var name)

toExpr :: GraphState -> NonEmpty (Term Void)
toExpr GraphState { stack, heap } =
  case stack <&> fromMaybe undefined . fromAddress heap of
    [] -> error "Mmm"
    x:xs -> x :| xs

visualize :: ([GraphState], GraphState) -> Doc ann
visualize (xs, y) = pretty (as, b) where
  as = xs <&> \x ->
    let e:|es = toExpr x
    in apps (Hole e :| fmap (over holes absurd) es)
  b = apps (toExpr y)

defs :: [Def]
defs =
  [ ("main", [], parseUnsafe parser "fix twice")
  , ("twice", ["a"], parseUnsafe parser "Pair a a")
  , ("fix", ["f"], parseUnsafe parser "f (fix f)")
  ]

steps :: GraphState -> ([GraphState], GraphState)
steps s = (s:) `first` either ([],) steps (step_ s)

-- TODO: add more language constructs
-- TODO: add garbage collection
