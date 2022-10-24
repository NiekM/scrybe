{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Types
import qualified Run
import RIO
import RIO.Process
import Options.Applicative.Simple
import qualified Paths_synthesis

main :: IO ()
main = runApp Run.run

runApp :: RIO Application () -> IO ()
runApp run = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_synthesis.version)
    "Header for command line arguments"
    "Program description, also for command line arguments"
    ( Options
      <$> switch
        ( long "verbose"
        <> short 'v'
        <> help "Verbose output?"
        )
      <*> strArgument
        ( metavar "INPUT"
        <> help "Program to synthesize"
        )
      <*> strOption
        ( long "prelude"
        <> short 'p'
        <> metavar "PRELUDE"
        <> value "data/prelude.hs"
        <> help "Prelude to use"
        )
      <*> option auto
        ( long "timeout"
        <> short 't'
        <> metavar "TIMEOUT"
        <> value 1000
        <> help "Maximum time spent in ms"
        )
    )
    empty
  lo <- logOptionsHandle stderr (view optVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = Application
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          }
     in runRIO app run
