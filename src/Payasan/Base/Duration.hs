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
  , Numeral(..)

  -- * Operations
  , isZero
  , isDotted
  , notDotted
  , dot
  , addDots
  , components
  , symbolicComponents
  , lilyPondComponents
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


import Text.PrettyPrint.HughesPJClass           -- package: pretty

import Data.Data
import Data.Ratio

data Numeral = D128   | D64     | D32     | D16
             | D8     | D4      | D2      | D1
             | Breve  | Longa   | Maxima
  deriving (Bounded,Data,Enum,Eq,Ord,Show,Typeable)



data Duration = DZero
              | Drn { _numeral :: Numeral, _dot_count :: Int }
  deriving (Data,Eq,Ord,Show,Typeable)


type RDuration = Rational


-- Pretty instances are for debugging and may not
-- correspond to valid output for LilyPond, ABC etc.


-- | Follow Humdrum which uses @q@ for durationless notes.
instance Pretty Duration where
  pPrint DZero          = char 'q'
  pPrint (Drn n dc)     = pPrint n <> text (replicate dc '.')

instance Pretty Numeral where
  pPrint D128           = int 128
  pPrint D64            = int 64
  pPrint D32            = int 32
  pPrint D16            = int 16
  pPrint D8             = int 8
  pPrint D4             = int 4
  pPrint D2             = int 2
  pPrint D1             = int 1
  pPrint Breve          = text "breve"
  pPrint Longa          = text "longa"
  pPrint Maxima         = text "maxima"



--------------------------------------------------------------------------------
-- Operations



-- | Zero durations do exist (the duration of a grace notes is officially
-- zero), however we ought not to be able to construct them.
--
isZero :: Duration -> Bool
isZero DZero = True
isZero _     = False


isDotted :: Duration -> Bool
isDotted DZero          = False 
isDotted (Drn _ dc)     = dc>0

-- more convenient to have this one...
notDotted :: Duration -> Bool
notDotted = not . isDotted
       
-- | Dot a duration. 
--
-- Note, @DZero@ an opaque value in the internal representation
-- cannot be dotted.
--
dot :: Duration -> Duration
dot DZero               = DZero
dot (Drn n dc)          = Drn n (dc+1)


addDots :: Int -> Duration -> Duration
addDots _ (DZero)       = DZero
addDots i (Drn n dc)    = Drn n (dc+i)



components :: Duration -> (Rational,Int)
components (DZero)              = (0,0)
components (Drn n dc)           = (toRat n,dc)

symbolicComponents :: Duration -> Maybe (Numeral,Int)
symbolicComponents (DZero)      = Nothing
symbolicComponents (Drn n dc)   = Just (n, dc)


lilyPondComponents :: Duration -> (Either String Int, Int)
lilyPondComponents (DZero)      = (Right 0, 0)
lilyPondComponents (Drn n dc)   = (fn n, dc)
  where
    fn D128      = Right 128
    fn D64       = Right 64
    fn D32       = Right 32
    fn D16       = Right 16
    fn D8        = Right 8
    fn D4        = Right 4
    fn D2        = Right 2
    fn D1        = Right 1
    fn Breve     = Left "breve"
    fn Longa     = Left "longa"
    fn Maxima    = Left "maxima"
    

-- | 'extent' - get the size of a Duration as a Rational 
-- (DurationMeasure).
--
durationSize :: Duration -> RDuration
durationSize (DZero)    = 0 
durationSize (Drn n dc) 
    | dc <= 0           = toRat n
    | otherwise         = let r = toRat n in step r (r/2) dc
  where
    step acc _ 0 = acc
    step acc h i = step (acc + h) (h/2) (i-1)

toRat :: Numeral -> Rational
toRat D128      = 1%128
toRat D64       = 1%64
toRat D32       = 1%32
toRat D16       = 1%16
toRat D8        = 1%8
toRat D4        = 1%4
toRat D2        = 1%2
toRat D1        = 1
toRat Breve     = 2
toRat Longa     = 4
toRat Maxima    = 8


-- | Convert a rational to a duration - dotting and double dotting
-- is supported.
--
rationalToDuration :: Rational -> Maybe Duration
rationalToDuration r 
    | r == 8%1      = Just $ Drn Maxima 0
    | r == 4%1      = Just $ Drn Longa 0
    | r == 2%1      = Just $ Drn Breve 0
    | r == 0        = Just $ DZero
    | r >  1        = Nothing
    | otherwise     = let (n,d) = (numerator r,denominator r)
                      in fn d >>= \base -> dotfun n base
  where
    dotfun i sym | i == 1    = Just $ Drn sym 0
                 | i == 3    = Just $ Drn (succ sym) 1
                 | i == 7    = Just $ Drn (succ $ succ sym) 2
                 | otherwise = Nothing
    fn 1   = Just $ D1
    fn 2   = Just $ D2
    fn 4   = Just $ D4
    fn 8   = Just $ D8
    fn 16  = Just $ D16
    fn 32  = Just $ D32
    fn 64  = Just $ D64
    fn 128 = Just $ D128
    fn _   = Nothing
    

--------------------------------------------------------------------------------
-- Named durations


dZero :: Duration
dZero = DZero

 
makeDuration :: Numeral -> Duration
makeDuration nm = Drn nm 0


dMaxima                         :: Duration
dMaxima                         = makeDuration Maxima

dLonga                          :: Duration
dLonga                          = makeDuration Longa

dBreve                          :: Duration
dBreve                          = makeDuration Breve

dWhole                          :: Duration
dWhole                          = makeDuration D1

dHalf                           :: Duration
dHalf                           = makeDuration D2

dQuarter                        :: Duration
dQuarter                        = makeDuration D4

dEighth                         :: Duration
dEighth                         = makeDuration D8

dSixteenth                      :: Duration
dSixteenth                      = makeDuration D16

dThirtySecondth                 :: Duration
dThirtySecondth                 = makeDuration D32

dSixtyFourth                    :: Duration
dSixtyFourth                    = makeDuration D64

dOneHundredAndTwentyEighth      :: Duration
dOneHundredAndTwentyEighth      = makeDuration D128
