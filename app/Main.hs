{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Import
import Run
import RIO.Process
import RIO.PrettyPrint.StylesUpdate
import Options.Applicative.Simple
import qualified Paths_synthesis

main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_synthesis.version)
    "Header for command line arguments"
    "Program description, also for command line arguments"
    (Options
       <$> switch ( long "verbose"
                 <> short 'v'
                 <> help "Verbose output?"
                  )
    )
    empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  let su = parseStylesUpdateFromString ""
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          , appStylesUpdate = su
          , appUseColor = True
          , appTermWidth = 180
          }
     in runRIO app run
