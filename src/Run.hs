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

-- A slightly more refined definition of map
mapDef2 :: Def
mapDef2 = Def
  { name = "map"
  , sig = TArr (TArr "a" "b") (TArr (list "a") (list "b"))
  , body = Sketch
    { expr = ELam "f" (TArr "a" "b")
      $ EApp (EApp (EVar (Left "foldr")) (EHole 0)) (EHole 1)
    , goals = Map.fromList
      [ (0, TArr "a" (TArr (list "b") (list "b")))
      , (1, list "b")
      ]
    }
  }

runSyn :: Env -> Def -> RIO App ()
runSyn env def = do
  logInfo "Sketch:"
  logInfo ""
  logInfo <=< displayWithColor . indent 2 $ pretty def
  logInfo ""
  logInfo "Possible refinements:"
  logInfo ""
  let syn = synthesize env (body def)
  let xss = takeWhile (not . null) . zip [0 :: Int ..] $ syn
  forM_ xss \(i, xs) -> do
    logInfo $ "Step: " <> fromString (show i)
    forM_ xs \x -> do
      d <- displayWithColor . expr $ sketch x
      logInfo ("  " <> d)

run :: RIO App ()
run = do
  runSyn prelude mapDef
  runSyn (Map.delete "foldr" prelude) mapDef2
  logInfo ""
  logInfo "Finished!"
