{-# LANGUAGE PolyKinds #-}

module Utils.Type where

import RIO
import Data.Kind

type family IsJust (a :: Maybe k) :: Bool where
  IsJust 'Nothing  = 'False
  IsJust ('Just _) = 'True

type family May (c :: k -> Constraint) (a :: Maybe k) :: Constraint where
  May c 'Nothing  = ()
  May c ('Just a) = c a

type family All (c :: k -> Constraint) (a :: [k]) :: Constraint where
  All c '[]       = ()
  All c (x ': xs) = (c x, All c xs)

type family Find (t :: k) (ts :: [k]) :: Maybe k where
  Find t '[]       = 'Nothing
  Find t (t ':  _) = 'Just t
  Find t (_ ': ts) = Find t ts

type Elem (t :: k) (ts :: [k]) = IsJust (Find t ts) ~ 'True
