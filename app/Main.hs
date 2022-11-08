{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Options
import qualified Run
import RIO
import RIO.Process
import Options.Applicative.Simple
import qualified Paths_scrybe

main :: IO ()
main = runApp Run.run

runApp :: RIO Application () -> IO ()
runApp run = do
  (options, cmd) <- simpleOptions
    $(simpleVersion Paths_scrybe.version)
    "Scrybe - Program Synthesis Using Example Propagation"
    ""
    ( Options
      <$> switch
        ( long "verbose"
        <> short 'v'
        <> help "Verbose output"
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
        <> help "Maximum time spent (in ms)"
        )
    )
    do
      addCommand "synth" "Synthesize a program from a sketch" id $
        Synth
        <$> strArgument ( metavar "INPUT"
          <> help "Program to synthesize"
          )
        <*>
        ( SynOptions
          <$> option auto
            ( long "propagate"
            <> value True
            <> help "Whether to prune using example propagation"
            )
        )
      addCommand "live" "Live evaluate an expression" Live $
        strArgument ( metavar "EXPR"
          <> help "Expression to live evaluate"
          )
      addCommand "assert"
        "Live unevaluate an expression onto a constraint" Assert $
        strArgument ( metavar "ASSERTION"
          <> help "The assertion to check"
          )
  lo <- logOptionsHandle stderr (view optVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = Application
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          , appCommand = cmd
          }
     in runRIO app run
