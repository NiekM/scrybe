module Run (run) where

import Import
import Test
import TermGen
import Lang

run :: RIO App ()
run = do
  logInfo "Sketch:"
  logInfo ""
  logInfo <=< displayWithColor . indent 2 $ pretty mapSketch
  logInfo ""
  logInfo "Possible hole fillings:"
  logInfo ""
  let xss = take 6 $ zip [0 :: Int ..] test
  forM_ xss \(i, xs) -> do
    logInfo $ "Step: " <> fromString (show i)
    forM_ xs \x -> do
      d <- displayWithColor (expr $ sketch x)
      logInfo ("  " <> d)
  logInfo ""
  logInfo "Finished!"
