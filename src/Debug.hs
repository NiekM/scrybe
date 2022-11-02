{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- NOTE: this module is purely for debugging purposes and to load some
-- functions and instances into ghci
module Debug where

import Import
import Language hiding (free)
import qualified RIO.Text as T
import System.IO.Unsafe
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import Data.SBV hiding (Logic)
import Run

fromStr :: Parse a => String -> a
fromStr = fromMaybe (error "Parse failed") . lexParse parser . T.pack

newtype Parseable a = Parseable a

instance Parse a => IsString (Parseable a) where
  fromString = Parseable . fromStr

deriving via Parseable (Expr l) instance Parse (Expr l) => IsString (Expr l)
deriving via Parseable Poly     instance IsString Poly
deriving via Parseable Assert   instance IsString Assert

prelude :: Env
prelude = let file = unsafePerformIO $ readFileUtf8 "data/prelude.hs" in
  case lexParse (fromDefs . recDefs <$> parser) file of
    Just x -> x
    Nothing -> error "Could not parse prelude"

tryUneval :: Uneval a -> Maybe a
tryUneval x = view _1 <$> runRWST x prelude 1000

uneval' :: Result -> Example -> Maybe (Logic Constraints)
uneval' r e = tryUneval (uneval r $ toEx e)

assert' :: Assert -> Maybe (Logic Constraints)
assert' = tryUneval . unevalAssert mempty

stripSuccs :: Example -> (SWord64, Bool)
stripSuccs = \case
  App (Ctr (MkCtr "Succ")) ex -> over _1 (+1) $ stripSuccs ex
  Ctr (MkCtr "Zero") -> (0, True)
  Hole (Unit ()) -> (0, False)
  _ -> error "Incorrect example"

type Cstr = ((Hole, Scope), Ex)

instance UnevalConstraint (Logic Cstr) where
  constr m h ex = pure ((h, m), ex)

sCstr :: Map (Hole, Scope) SWord64 -> Logic Cstr -> Symbolic SBool
sCstr hs = \case
  Conjunction xs -> sAnd <$> for xs (sCstr hs)
  Disjunction xs -> sOr  <$> for xs (sCstr hs)
  Pure (hm, ex) -> case Map.lookup hm hs of
    Nothing -> error "Missing hole-scope pair"
    Just i -> return . sAnd $ fromEx ex <&> \x -> case stripSuccs x of
      (n, False) -> i .>= n
      (n, True ) -> i .== n

sAss :: Assert -> Symbolic SBool
sAss (MkAssert e ex) = do
  let r = runEval prelude $ eval mempty e
  let c = fromMaybe (error "err") $ runUneval prelude 1000 $ uneval r (toEx ex)
  let hm = over _2 Set.singleton . view _1 <$> toList c
  let hm' = Map.fromListWith Set.union hm
  hms <- Map.unions . concat <$> for (Map.assocs hm') \(h, ms) -> do
    for (zip [0 :: Int ..] $ Set.toList ms) \(i, m) -> do
      x <- free ("h_" ++ show (pretty h) ++ '.' : show i)
      return $ Map.singleton (h, m) x
  sCstr hms c
