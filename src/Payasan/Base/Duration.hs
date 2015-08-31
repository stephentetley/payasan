{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Duration
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Symbolic (not numeric) representation of duration.
--
-- Import directly if needed.
--
--------------------------------------------------------------------------------

module Payasan.Base.Duration
  (
    Duration
  , RDuration

  -- * Operations
  , isZero
  , isDotted
  , notDotted
  , dot
  , addDots
  , components
  , durationSize
  , rationalToDuration

  -- * Named durations
  , dZero
  , dMaxima
  , dLonga
  , dBreve
  , dWhole
  , dHalf
  , dQuarter
  , dEighth
  , dSixteenth
  , dThirtySecondth
  , dSixtyFourth
  , dOneHundredAndTwentyEighth
  ) where

import Data.Data
import Data.Ratio

data Numeral = N128   | N64     | N32     | N16
             | N8     | N4      | N2      | N1
             | Breve  | Longa   | Maxima
  deriving (Bounded,Data,Enum,Eq,Ord,Show,Typeable)



data Duration = DZero
              | D1 { _dNumeral :: Numeral, _dotCount :: Int }
  deriving (Data,Eq,Ord,Show,Typeable)


type RDuration = Rational

--------------------------------------------------------------------------------
-- Operations



-- Zero durations do exist (the duration of a grace notes is officially
-- zero), however we ought not to be able to construct them.
isZero :: Duration -> Bool
isZero DZero = True
isZero _     = False


isDotted :: Duration -> Bool
isDotted DZero     = False 
isDotted (D1 _ dc) = dc>0

-- more convenient to have this one...
notDotted :: Duration -> Bool
notDotted = not . isDotted
       
-- | Dot a duration. 
--
-- Note, @DZero@ an opaque value in the internal representation
-- cannot be dotted.
--
dot :: Duration -> Duration
dot DZero     = DZero
dot (D1 n dc) = D1 n (dc+1)


addDots :: Int -> Duration -> Duration
addDots _ DZero     = DZero
addDots i (D1 n dc) = D1 n (dc+i)



components :: Duration -> (Rational,Int)
components DZero        = (0,0)
components (D1 n dc) = (toRat n,dc)


-- | 'extent' - get the size of a Duration as a Rational 
-- (DurationMeasure).
--
durationSize :: Duration -> RDuration
durationSize DZero      = 0 
durationSize (D1 n dc) 
    | dc <= 0           = toRat n
    | otherwise         = let r = toRat n in step r (r/2) dc
  where
    step acc _ 0 = acc
    step acc h i = step (acc + h) (h/2) (i-1)

toRat :: Numeral -> Rational
toRat N128      = 1%128
toRat N64       = 1%64
toRat N32       = 1%32
toRat N16       = 1%16
toRat N8        = 1%8
toRat N4        = 1%4
toRat N2        = 1%2
toRat N1        = 1
toRat Breve     = 2
toRat Longa     = 4
toRat Maxima    = 8


-- | Convert a rational to a duration - dotting and double dotting
-- is supported.
--
rationalToDuration :: Rational -> Maybe Duration
rationalToDuration r 
    | r == 8%1      = Just $ D1 Maxima 0
    | r == 4%1      = Just $ D1 Longa 0
    | r == 2%1      = Just $ D1 Breve 0
    | r == 0        = Just $ DZero
    | r >  1        = Nothing
    | otherwise     = let (n,d) = (numerator r,denominator r)
                      in fn d >>= \base -> dotfun n base
  where
    dotfun i sym | i == 1    = Just $ D1 sym 0
                 | i == 3    = Just $ D1 (succ sym) 1
                 | i == 7    = Just $ D1 (succ $ succ sym) 2
                 | otherwise = Nothing
    fn 1   = Just $ N1
    fn 2   = Just $ N2
    fn 4   = Just $ N4
    fn 8   = Just $ N8
    fn 16  = Just $ N16
    fn 32  = Just $ N32
    fn 64  = Just $ N64
    fn 128 = Just $ N128
    fn _   = Nothing
    

--------------------------------------------------------------------------------
-- Named durations


dZero :: Duration
dZero = DZero

 
makeDuration :: Numeral -> Duration
makeDuration nm = D1 nm 0


dMaxima                         :: Duration
dMaxima                         = makeDuration Maxima

dLonga                          :: Duration
dLonga                          = makeDuration Longa

dBreve                          :: Duration
dBreve                          = makeDuration Breve

dWhole                          :: Duration
dWhole                          = makeDuration N1

dHalf                           :: Duration
dHalf                           = makeDuration N2

dQuarter                        :: Duration
dQuarter                        = makeDuration N4

dEighth                         :: Duration
dEighth                         = makeDuration N8

dSixteenth                      :: Duration
dSixteenth                      = makeDuration N16

dThirtySecondth                 :: Duration
dThirtySecondth                 = makeDuration N32

dSixtyFourth                    :: Duration
dSixtyFourth                    = makeDuration N64

dOneHundredAndTwentyEighth      :: Duration
dOneHundredAndTwentyEighth      = makeDuration N128
