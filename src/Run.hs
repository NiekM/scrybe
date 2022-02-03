{-# LANGUAGE QuasiQuotes #-}
module Run (run) where

import Import
import TermGen
import Language.Parser
import Language.Prelude
import Language.Syntax
import Language.Utils
import qualified RIO.Map as Map

mapBinding :: Binding Hole
mapBinding = [bi|map :: (a -> b) -> List a -> List b|]

mapBody :: Sketch Hole
mapBody = Sketch
  { expr = [ex|\f :: a -> b. {0}|]
  , goals = Map.singleton 0 [ty|List a -> List b|]
  }

-- A slightly more refined definition of map
mapBody2 :: Sketch Hole
mapBody2 = Sketch
  { expr = [ex|\f :: a -> b. foldr {0} {1}|]
  , goals = Map.fromList
    [ (0, [ty|a -> List b -> List b|])
    , (1, [ty|List b|])
    ]
  }

runSyn :: [Binding Hole] -> Binding Hole -> Sketch Hole -> RIO App ()
runSyn ctx binding body = do
  let env = mkEnv ctx
  logInfo "Sketch:"
  logInfo ""
  logInfo . display . indent 2 $
    pretty binding <> hardline <> "  =" <+> pretty body
  logInfo ""
  logInfo "Possible refinements:"
  logInfo ""
  let syn = synthesize env body
  let xss = takeWhile (not . null) . zip [0 :: Int ..] $ syn
  forM_ xss \(i, xs) -> do
    logInfo $ "Step: " <> fromString (show i)
    forM_ xs (logInfo . display . indent 2 . pretty . expr . sketch)

run :: RIO App ()
run = do
  runSyn prelude mapBinding mapBody
  runSyn (filter (\(Binding n _) -> n /= "foldr") prelude) mapBinding mapBody2
  logInfo ""
  logInfo "Finished!"
