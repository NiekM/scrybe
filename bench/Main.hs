module Main where

import Import hiding (timeout)
import Synthesis
import Language.Parser
import Language.Syntax hiding (env)
import RIO.FilePath
import RIO.Directory
import Criterion.Main
import Control.Monad.Heap hiding (Leaf)
import System.Timeout

data Tree a b = Node a [Tree a b] | Leaf b
  deriving (Eq, Ord, Show, Read)
  deriving (Functor, Foldable, Traversable)

type FileTree = Tree String (String, Defs Unit)

removeMaybes :: Tree a (Maybe b) -> Maybe (Tree a b)
removeMaybes = \case
  Node x xs -> case mapMaybe removeMaybes xs of
    [] -> Nothing
    ys -> Just $ Node x ys
  Leaf l -> Leaf <$> l

trySyn :: Env -> Defs Unit -> Bool
trySyn m = isJust . best . runNondet . runSynth m . synth

getTree :: MonadIO m => FilePath -> m [FileTree]
getTree p = do
  fs <- listDirectory p
  catMaybes <$> forM fs \f -> case splitExtensions f of
    (d, "") -> do
      t <- getTree (p </> d)
      return . Just $ Node d t
    (a, ".hs") -> do
      t <- readFileUtf8 $ p </> f
      let x = fromMaybe undefined . lexParse parser $ t
      return . Just $ Leaf (a, x)
    (_, _) -> return Nothing

benchTree :: Env -> FileTree -> Benchmark
benchTree m = \case
  Node x xs -> bgroup x $ benchTree m <$> xs
  Leaf (f, x) -> bench f $ nf (trySyn m) x

-- TODO: use `env' function to read input
-- TODO: give better control over which benchmarks are tried and checked for
-- timeouts, now all benchmarks are checked for timeout, not just the ones we
-- want to run.
main :: IO ()
main = do
  pre <- readFileUtf8 "data/prelude.hs"
  let m = maybe undefined (fromDefs . recDefs) $ lexParse parser pre
  let benchmarks = "data/benchmarks"
  t <- getTree benchmarks
  t' <- for t $ mapM \(s, x) -> timeout 5000000 (return $! force (trySyn m x))
    <&> \case
      Nothing -> Nothing
      Just _ -> Just (s, x)
  let t'' = mapMaybe removeMaybes t'
  defaultMain $ benchTree m <$> t''
