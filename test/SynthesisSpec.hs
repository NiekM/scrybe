module SynthesisSpec where

import Import
import Synthesis
import Test.Hspec
import Language.Parser
import RIO.FilePath
import RIO.Directory
import Control.Monad.Heap

spec :: Spec
spec = do
  describe "benchmark" do
    pre <- runIO $ readFileUtf8 "data/prelude.hs"
    let m = fromMaybe undefined $ lexParse parser pre
    let benchmarks = "data/benchmarks"
    fs <- runIO $ listDirectory benchmarks
    xs <- for fs \f -> do
      t <- runIO . readFileUtf8 $ benchmarks </> f
      let x = fromMaybe undefined . lexParse parser $ t
      return (f, x)
    for_ xs \(f, x) -> describe (takeBaseName f) do
      it "synthesizes" . isJust . best . runSynth m $ synth x

-- TODO: do benchmarking using the criterion package
