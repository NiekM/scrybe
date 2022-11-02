module SynthesisSpec where

import Import
import Synthesis
import Test.Hspec
import Language.Defs
import Language.Parser
import Language.Syntax
import Run
import RIO.FilePath
import RIO.Directory
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

specTree :: Env -> FileTree -> Spec
specTree m = \case
  Node x xs -> describe x . for_ xs $ specTree m
  Leaf (f, x) -> describe f do
    it "synthesizes" . isJust . best . runNondet . runSynth m $ synth x

spec :: Spec
spec = do
  pre <- runIO $ readFileUtf8 "data/prelude.hs"
  let m = maybe undefined (fromDefs . recDefs) $ lexParse parser pre
  let benchmarks = "data/benchmarks"
  t <- runIO $ getTree benchmarks
  specTree m $ Node "benchmarks" t
