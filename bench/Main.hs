{-# LANGUAGE NumericUnderscores #-}

module Main where

import Import hiding (timeout)
import Synthesis
import Options
import Options.Applicative
import Language.Parser
import Language.Syntax
import Language.Defs
import Language.Live
import Utils.Weighted
import RIO.FilePath
import RIO.Directory
import Criterion
import Criterion.Internal
import Criterion.Main
import Criterion.Main.Options
import Criterion.Monad
import Criterion.Report
import Criterion.Types
import Statistics.Types
import System.IO
import Control.Monad.Heap hiding (Leaf)
import System.Timeout
import Text.Printf
import qualified RIO.Text as Text
import qualified RIO.Text.Partial as Text
import qualified RIO.Map as Map

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

trySyn :: Bool -> Env -> Defs Unit -> Maybe (Term Hole, Fillings)
trySyn b m = fmap snd . best . runSearch . runSynth (SynOptions b) m . synth

allFiles :: MonadIO m => FilePath -> m [(String, Text)]
allFiles p = do
  fs <- listDirectory p
  concat <$> forM fs \f -> case splitExtensions f of
    (d, "") -> allFiles (p </> d) <&> over (each . _1) ((d++). ('_':))
    (a, ".hs") -> do
      t <- readFileUtf8 $ p </> f
      return [(a, t)]
    (_, _) -> return []

single :: String -> Config -> Int -> Env -> Defs Unit -> Bool ->
  IO (Maybe (Maybe Report))
single a cfg t p d b = timeout t (syn `deepseq` return syn) >>= \case
  Nothing -> return Nothing
  Just Nothing -> return $ Just Nothing
  Just (Just (expr, hf)) -> do
    for_ (relevant expr) \(MkBinding x e) -> do
      print . pretty $ MkBinding x (fill (normalizeFilling hf) e)
    let name = (if b then "(yes) " else "(no) ") <> a
    withConfig cfg $ do
      Analysed r <- runAndAnalyseOne 0 name $ nf (trySyn b p) d
      return . Just $ Just r
  where syn = trySyn b p d

-- TODO: use `env' function to read input
-- TODO: give better control over which benchmarks are tried and checked for
-- timeouts, now all benchmarks are checked for timeout, not just the ones we
-- want to run.
main :: IO ()
main = do
  let def = defaultConfig { verbosity = Quiet }
  cfg <- execParser $ info (config def) briefDesc
  pre <- readFileUtf8 "data/prelude.hs"
  let p = maybe undefined (fromDefs . recDefs) $ lexParse parser pre
  benchInfo <- readFileUtf8 "data/benchmarks/.info"
  let
    m = Map.fromList $ (Text.words <$> Text.lines benchInfo) <&> \case
      x:xs -> (x, xs)
      _ -> undefined
    benchmarks = "data/benchmarks"
  ts <- allFiles benchmarks
  xs <- for (reverse ts) \(a, x) -> do
    let d = fromMaybe undefined $ lexParse parser x
    let t = 10_000_000
    putStrLn ""
    putStrLn $ "Synthesizing " <> a <> "..."
    putStrLn "...with example propagation:"
    r0 <- single a cfg t p d True
    putStrLn "...without example propagation:"
    r1 <- single a cfg t p d False
    return (format (fromString a) d r0 r1 m, r0, r1)
  let ls = view _1 <$> xs
  let rs = concatMap (\(_,a,b) -> mapMaybe join [a,b]) xs
  putStrLn ""
  putStrLn . Text.unpack . Text.concat $ ls
  withConfig cfg $ report rs

format
  :: Text
  -> Defs Unit
  -> Maybe (Maybe Report)
  -> Maybe (Maybe Report)
  -> Map Text [Text]
  -> Text
format s d r0 r1 m = case Map.lookup s m of
  Just [tag, myth, smyth, lam] -> Text.intercalate " & "
    [ Text.replace "_" "\\_" s <> tag
    , Text.replace "`" "$" desc
    , time r0
    , time r1
    , myth
    , smyth
    , lam
    ] <> "\\\\\n"
      where
      desc = mconcat [x | Desc x <- pragmas d ]
      time = maybe "$\\bot$" (maybe "-" $ Text.pack . printf "%.2f" . mean)
      mean = (1000*) . estPoint . anMean . reportAnalysis
  _ -> error "Missing formatting info"
