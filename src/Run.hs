{-# LANGUAGE QuasiQuotes #-}
module Run (run) where

import Import
import TermGen
import Language.Parser
import Language.Prelude
import Language.Syntax
import Language.Utils

mapSketch :: Expr (Type Void)
mapSketch = [ext|\f :: a -> b. {List a -> List b}|]

mapSketch2 :: Expr (Type Void)
mapSketch2 = [ext|\f :: a -> b. folr {a -> List b -> List b} {List b}|]

runSyn :: [Binding Hole] -> Expr (Type Void) -> RIO App ()
runSyn ctx body = do
  let env = mkEnv ctx
  logInfo "Sketch:"
  logInfo ""
  logInfo . display . indent 2 $ pretty body
  logInfo ""
  logInfo "Possible refinements:"
  logInfo ""
  let syn = synthesize env body
  let xss = takeWhile (not . null) . zip [0 :: Int ..] $ syn
  forM_ xss \(i, xs) -> do
    logInfo $ "Step: " <> fromString (show i)
    forM_ xs (logInfo . display . indent 2 . pretty . expr)

run :: RIO App ()
run = do
  runSyn prelude mapSketch
  runSyn (filter (\(Binding n _) -> n /= "foldr") prelude) mapSketch2
  logInfo ""
  logInfo "Finished!"
