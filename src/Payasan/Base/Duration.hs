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
  , doubleDuration
  , halveDuration

  , components
  , symbolicComponents
  , lilyPondComponents
  , toRDuration
  , rationalToDuration

  -- * Named durations
  , d_zero
  , d_maxima
  , d_longa
  , d_breve
  , d_whole
  , d_half
  , d_quarter
  , d_eighth
  , d_sixteenth
  , d_thirty_secondth
  , d_sixty_fourth
  , d_one_hundred_and_twenty_eighth
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


-- | Double the duration.
--
-- @Maxima@ is /saturated/:
--
-- > doubleDuration Maxima = Maxima
--
doubleDuration :: Duration -> Duration
doubleDuration (DZero)      = DZero
doubleDuration (Drn n dc)   = Drn (fn n) dc
  where
    fn D128      = D64
    fn D64       = D32
    fn D32       = D16
    fn D16       = D8
    fn D8        = D4
    fn D4        = D2
    fn D2        = D1
    fn D1        = Breve
    fn Breve     = Longa
    fn Longa     = Maxima
    fn Maxima    = Maxima
    
-- | Halve the duration (diminution).
-- 
-- D128 is /saturated/:
-- 
-- > halveDuration D128 = D128
--
halveDuration :: Duration -> Duration
halveDuration (DZero)      = DZero
halveDuration (Drn n dc)   = Drn (fn n) dc
  where
    fn D128      = D128
    fn D64       = D128
    fn D32       = D64
    fn D16       = D32
    fn D8        = D16
    fn D4        = D8
    fn D2        = D4
    fn D1        = D2
    fn Breve     = D1
    fn Longa     = Breve
    fn Maxima    = Longa

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
toRDuration :: Duration -> RDuration
toRDuration (DZero)    = 0 
toRDuration (Drn n dc) 
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



 
makeDuration :: Numeral -> Duration
makeDuration nm = Drn nm 0


d_zero                              :: Duration
d_zero                              = DZero

d_maxima                            :: Duration
d_maxima                            = makeDuration Maxima

d_longa                             :: Duration
d_longa                             = makeDuration Longa

d_breve                             :: Duration
d_breve                             = makeDuration Breve

d_whole                             :: Duration
d_whole                             = makeDuration D1

d_half                              :: Duration
d_half                              = makeDuration D2

d_quarter                           :: Duration
d_quarter                           = makeDuration D4

d_eighth                            :: Duration
d_eighth                            = makeDuration D8

d_sixteenth                         :: Duration
d_sixteenth                         = makeDuration D16

d_thirty_secondth                   :: Duration
d_thirty_secondth                   = makeDuration D32

d_sixty_fourth                      :: Duration
d_sixty_fourth                      = makeDuration D64

d_one_hundred_and_twenty_eighth     :: Duration
d_one_hundred_and_twenty_eighth     = makeDuration D128
