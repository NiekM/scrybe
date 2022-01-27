module Run (run) where

import Import
import Test
import TermGen
import Lang
import qualified RIO.Map as Map

mapDef :: Def
mapDef = Def
  { name = "map"
  , sig = TArr (TArr "a" "b") (TArr (list "a") (list "b"))
  , body = Sketch
    { expr = ELam "f" (TArr "a" "b") (EHole 0)
    , goals = Map.singleton 0 (TArr (list "a") (list "b"))
    }
  }

run :: RIO App ()
run = do
  let def = mapDef
  logInfo "Sketch:"
  logInfo ""
  logInfo <=< displayWithColor . indent 2 $ pretty def
  logInfo ""
  logInfo "Possible refinements:"
  logInfo ""
  let syn = synthesize prelude (body def)
  let xss = takeWhile (not . null) . zip [0 :: Int ..] $ syn
  forM_ xss \(i, xs) -> do
    logInfo $ "Step: " <> fromString (show i)
    forM_ xs \x -> do
      d <- displayWithColor . expr $ sketch x
      logInfo ("  " <> d)
  logInfo ""
  logInfo "Finished!"
