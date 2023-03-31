module Constraint (Cstr(..), InOut(..), normCstr, checkNorm, minimal) where

import Import
import Language.Syntax
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Prettyprinter as Pretty

newtype Par = Par Var
  deriving (Eq, Ord, Show)

newtype Positions = Positions (Map Par (Set Position))
  deriving stock (Eq, Ord, Show)

instance Semigroup Positions where
  Positions x <> Positions y = Positions (Map.unionWith Set.union x y)

instance Monoid Positions where
  mempty = Positions Map.empty

par :: Ctr -> Maybe Par
par c@(MkCtr t)
  | c `elem` ["A", "B", "C", "D", "E"] = Just (Par (MkVar t))
  | otherwise = Nothing

normInput :: Position -> Value -> (Positions, Value)
normInput p = \case
  Ctr c [] | Just t <- par c ->
    (Positions (Map.singleton t (Set.singleton p)), Ctr "_" [])
  Ctr c xs ->
    let ps = flip (Prj c) p <$> [0..]
    in Ctr c <$> normInputs (zip ps xs)

normInputs :: Traversable t => t (Position, Value) -> (Positions, t Value)
normInputs = traverse $ uncurry normInput

normOutput :: Positions -> Value -> ([Set Position], Value)
normOutput ps@(Positions m) = \case
  Ctr c [] | Just t <- par c ->
    ([fromMaybe Set.empty (Map.lookup t m)], Ctr "_" [])
  Ctr c xs -> Ctr c <$> traverse (normOutput ps) xs

data InOut = InOut [Value] Value
  deriving (Eq, Ord, Show)

data Cstr = Cstr [Var] [InOut]
  deriving (Eq, Ord, Show)

data Norm = Norm [Var] [(InOut, [Set Position])]
  deriving (Eq, Ord, Show)

normCstr :: Cstr -> Norm
normCstr (Cstr as xs) = Norm as (map norm xs)
  where
    norm (InOut is o) = (InOut is' o', vs)
      where
        rs = map Var as
        (ps, is') = normInputs (zip rs is)
        (vs, o') = normOutput ps o

checkNorm :: Norm -> Maybe Norm
checkNorm (Norm as xs) = do
  let
    ys = map (\(InOut is o, ps) -> (is, Just (o, ps))) xs

  m <- fmap Map.assocs . sequence $ flip Map.fromListWith ys \x y -> do
    (a, ps) <- x
    (b, qs) <- y
    guard $ a == b
    let rs = zipWith Set.intersection ps qs
    return (a, rs)

  guard . not . any null $ view (each . _2 . _2) m
  return $ Norm as $ m <&> \(is, (o, ps)) -> (InOut is o, ps)

restrict :: Set Var -> Cstr -> Cstr
restrict vs (Cstr as xs) =
  Cstr (filter (`elem` vs) as) $ xs <&> \(InOut is o) ->
    InOut [i | (a, i) <- zip as is, a `elem` vs] o

check :: Cstr -> Bool
check = isJust . checkNorm . normCstr

minimal :: Cstr -> [Set Var]
minimal cs@(Cstr as _) = go ps
  where
    ps = List.sortOn length . Set.toList $ Set.powerSet (Set.fromList as)
    go [] = []
    go (x:xs)
      | check (restrict x cs) = x : go (filter (not . Set.isSubsetOf x) xs)
      | otherwise = go xs

instance Pretty InOut where
  pretty (InOut is o) =
    Pretty.hsep (prettyValuePrec 3 <$> is) <+> "->" <+> pretty o

instance Pretty Cstr where
  pretty (Cstr as xs) = Pretty.align . Pretty.vsep $
    Pretty.hsep (pretty <$> as) : fmap pretty xs

instance Pretty Norm where
  pretty (Norm as xs) = Pretty.align . Pretty.vsep $
    Pretty.hsep (pretty <$> as) : fmap p xs
    where p (io, ps) = pretty io <+> Pretty.list (pretty <$> ps)

-- TODO: how to represent recursive constraints?
-- TODO: how to represent non-determinism?

a0, a1, a2 :: Value
a0 = Ctr "A" []
a1 = Ctr "B" []
a2 = Ctr "C" []

constant :: Cstr
constant = Cstr ["xs", "x"] [InOut [list [a0, a1, a2], a0] (nat 1)]

rev :: Cstr
rev = Cstr ["xs", "x"]
  [ InOut [list [a0, a1, a2], a0] a2
  , InOut [list [a0, a1, a2], a1] a1
  , InOut [list [a0, a1, a2], a2] a0
  ]

revfoldr :: Cstr
revfoldr = Cstr ["xs", "x", "r"]
  [ InOut [list [a0, a1, a2], a0, list [a2, a1]] (list [a2, a1, a0])
  , InOut [list [a0, a1, a2], a1, list [a2]] (list [a2, a1])
  , InOut [list [a0, a1, a2], a2, list []] (list [a2])
  ]

test :: Cstr
test = Cstr ["x", "y", "z"]
  [ InOut [a0, a1, a1] a1
  , InOut [a1, a0, a1] a1
  , InOut [a1, a1, a0] a1
  ]

testx :: Cstr
testx = Cstr ["x", "y", "z"]
  [ InOut [a0, a1, a1] a0
  , InOut [a1, a0, a1] a1
  , InOut [a1, a1, a0] a1
  ]

testy :: Cstr
testy = Cstr ["x", "y", "z"]
  [ InOut [a0, a1, a1] a1
  , InOut [a1, a0, a1] a0
  , InOut [a1, a1, a0] a1
  ]

testz :: Cstr
testz = Cstr ["x", "y", "z"]
  [ InOut [a0, a1, a1] a1
  , InOut [a1, a0, a1] a1
  , InOut [a1, a1, a0] a0
  ]

len :: Cstr
len = Cstr ["xs"]
  [ InOut [list []] (nat 0)
  , InOut [list [a0]] (nat 1)
  , InOut [list [a0, a1]] (nat 2)
  , InOut [list [a0, a1, a2]] (nat 3)
  ]

lenfoldr :: Cstr
lenfoldr = Cstr ["xs", "x", "r"]
  [ InOut [list [a0], a0, nat 0] (nat 1)
  , InOut [list [a0, a1], a0, nat 1] (nat 2)
  , InOut [list [a0, a1, a2], a0, nat 2] (nat 3)
  ]
