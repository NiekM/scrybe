module Main where

import Import
import Synthesis
import Language.Parser
import Language.Syntax
import RIO.FilePath
import RIO.Directory
import Criterion.Main
import Control.Monad.Heap hiding (Leaf)

data Tree a b = Node a [Tree a b] | Leaf b
  deriving (Eq, Ord, Show, Read)
  deriving (Functor, Foldable, Traversable)

type FileTree = Tree String (String, Defs Unit)

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
  Leaf (f, x) -> bench f $
    whnf (isJust . best . runSynth m . synth) x

main :: IO ()
main = do
  pre <- readFileUtf8 "data/prelude.hs"
  let m = fromMaybe undefined $ lexParse parser pre
  let benchmarks = "data/benchmarks"
  t <- getTree benchmarks
  defaultMain $ benchTree m <$> t
