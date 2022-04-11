{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Eval where

import Import
import Language.Syntax
import RIO.List (splitAt, maximumMaybe)
import qualified RIO.Map as Map
import Control.Monad.State
import Prettyprinter
import Language.Parser

class HasEnv' a where
  env' :: Lens' a (Map Var (Term Var Void))

instance HasEnv' (Map Var (Term Var Void)) where
  env' = id

eval' :: Map Var (Term Var Void) -> Term Var Void -> Maybe (Term Var Void)
eval' m e = evalStateT (eval e) m

-- | A simple evaluator/normalizer for expressions that leaves subexpressions
-- as if when they cannot be evaluated further.
-- TODO: add alpha renaming
eval :: (MonadFail m, MonadState s m, HasEnv' s) =>
  Term Var Void -> m (Term Var Void)
eval = \case
  Hole h -> return $ Hole h
  Var x -> do
    m <- use env'
    case Map.lookup x m of
      Nothing -> fail $ "Unknown variable " <> show x
      Just e -> eval e
  Ctr c -> return $ Ctr c
  App f x -> do
    g <- eval f
    y <- eval x
    case g of
      Lam a z -> do
        modifying env' $ Map.insert a y
        eval z
      Elim xs -> do
        Apps (Ctr c) as <- eval x
        case lookup c xs of
          Just e -> eval $ apps e as
          Nothing -> fail "Pattern match failure"
      _ -> return $ App g y
  Lam a x -> return $ Lam a x
  Let a x e -> do
    y <- eval x
    modifying env' $ Map.insert a y
    eval e
  Elim xs -> return $ Elim xs

type Address = Int
type Body = Expr 'Term Var Void

type Node = Expr' ('Base Address) 'Term Var Void

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
          let (b, h) = runState (inst (Map.fromList (zip xs as) <> global) e) heap
          in return $ GraphState (b:s') h global funs
        | otherwise -> Left g
      _ -> Left g

alloc :: Node -> State Heap Address
alloc n = do
  h <- get
  let i = maybe 0 (+1) $ maximumMaybe (Map.keysSet h)
  put $ Map.insert i n h
  return i

inst :: Global -> Body -> State Heap Address
inst g = cataExprM \case
  Var a -> maybe (error "Oh oh") return $ Map.lookup a g
  Hole i -> absurd i
  e -> alloc e

initialHeap :: [Def] -> (Heap, Global, Funs)
initialHeap = foldr (\(x, as, b) (h, g, f) ->
  let (g', h') = runState (allocate (x, as, b)) h
  in (h', g', Map.singleton x (as, b)) <> (h, g, f)) mempty

compile :: [Def] -> GraphState
compile program = GraphState { stack, heap, global, funs } where
  stack = [Map.findWithDefault (error "Oh no") "main" global]
  (heap, global, funs) = initialHeap program

fromAddress :: Heap -> Address -> Maybe (Term Var Void)
fromAddress = anaExprM . flip Map.lookup

allocate :: Def -> State Heap Global
allocate (name, _, _) = do
  address <- alloc $ Var name
  return $ Map.singleton name address

toExpr :: GraphState -> (Term Var Void, [Term Var Void])
toExpr GraphState { stack, heap } =
  case stack <&> fromMaybe undefined . fromAddress heap of
    [] -> error "Mmm"
    x:xs -> (x, xs)

visualize :: ([GraphState], GraphState) -> Doc ann
visualize (xs, y) = pretty (as, b) where
  as = xs <&> \x ->
    let (e, es) = toExpr x
    in apps (Hole e) (fmap (over holes absurd) es)
  b = uncurry apps . toExpr $ y

parseUnsafe :: Parse a => Text -> a
parseUnsafe = fromMaybe undefined . lexParse parser

defs :: [Def]
defs =
  [ ("main", [], parseUnsafe "fix twice")
  , ("twice", ["a"], parseUnsafe "Pair a a")
  , ("fix", ["f"], parseUnsafe "f (fix f)")
  ]

steps :: GraphState -> ([GraphState], GraphState)
steps s = (s:) `first` either ([],) steps (step_ s)

-- TODO: add more language constructs
-- TODO: add garbage collection
