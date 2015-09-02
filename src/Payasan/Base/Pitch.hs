{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Pitch
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Pitch type
--
-- Import directly if needed.
-- 
-- z- prefix on functions indicates they operate on PitchSpelling
-- which has no notion of octave (c.f. Z12 modulo representation).
--
--------------------------------------------------------------------------------

module Payasan.Base.Pitch
  ( 

  -- * Pitch
    Pitch(..)
  , PitchSpelling(..)
  , PitchLetter(..)
  , Alteration(..)
  , Octave


  , middle_c

  , naturalOf
  , znaturalOf
  , sharpOf
  , zsharpOf
  , flatOf
  , zflatOf
  , semitoneCount
  , midiSemitoneCount
  , zsemitoneCount
  , isHigher
  , isLower
  , equivalent
  , zequivalent

  
  , arithmeticDistance
  , zarithmeticDistance

  , semitoneDistance
  , semitonesToNext
  , semitonesToPrev

  , octaveDistance

  -- * Intervals
  , IntervalType(..)
  , Interval(..)
  , intervalBetween
  , addInterval
  , subInterval

  , isSmaller
  , isLarger
  , octaveCount
  , addOctaves
  , arithDistModulo

  , simpleIntervalOf

  , invertSimpleInterval

  , description

  )
  where


import Data.Data


-- | middle c is c4
data Pitch = Pitch 
    { pitch_spelling    :: !PitchSpelling 
    , pitch_octave      :: !Octave 
    }
  deriving (Data,Eq,Ord,Show,Typeable)

data PitchSpelling = PitchSpelling !PitchLetter !Alteration
  deriving (Data,Eq,Ord,Show,Typeable)


data PitchLetter = C | D | E | F | G | A | B 
  deriving (Bounded,Data,Enum,Eq,Ord,Show,Typeable)

data Alteration = DBL_FLAT | FLAT | NAT | SHARP | DBL_SHARP
  deriving (Data,Enum,Eq,Ord,Show,Typeable)


type Octave = Int

-- | Middle C is octave 4 as per /scientific notation/.
--
middle_c :: Pitch
middle_c = Pitch (PitchSpelling C NAT) 4


--------------------------------------------------------------------------------
-- Operations


naturalOf :: Pitch -> Pitch
naturalOf (Pitch s o) = Pitch (znaturalOf s) o

znaturalOf :: PitchSpelling -> PitchSpelling
znaturalOf (PitchSpelling l _) = PitchSpelling l NAT

sharpOf :: Pitch -> Pitch
sharpOf (Pitch s o) = Pitch (zsharpOf s) o

zsharpOf :: PitchSpelling -> PitchSpelling
zsharpOf (PitchSpelling l _) = PitchSpelling l SHARP

flatOf :: Pitch -> Pitch
flatOf (Pitch s o) = Pitch (zflatOf s) o

zflatOf :: PitchSpelling -> PitchSpelling
zflatOf (PitchSpelling l _) = PitchSpelling l FLAT


-- | Middle C is 48 - to get MIDI semitone count add 12
--
semitoneCount :: Pitch -> Int
semitoneCount (Pitch s o) = zsemitoneCount s + (12 * o)

-- | Middle C is 48 - to get MIDI semitone count add 12
--
midiSemitoneCount :: Pitch -> Int
midiSemitoneCount p = 12 + semitoneCount p



zsemitoneCount :: PitchSpelling -> Int
zsemitoneCount (PitchSpelling pl a) = semitoneCountPL pl + semitoneCountA a


semitoneCountPL :: PitchLetter -> Int
semitoneCountPL C = 0
semitoneCountPL D = 2
semitoneCountPL E = 4
semitoneCountPL F = 5
semitoneCountPL G = 7
semitoneCountPL A = 9
semitoneCountPL B = 11

semitoneCountA :: Alteration -> Int
semitoneCountA DBL_FLAT  = -2
semitoneCountA FLAT      = -1
semitoneCountA NAT       = 0
semitoneCountA SHARP     = 1
semitoneCountA DBL_SHARP = 2


isHigher :: Pitch -> Pitch -> Bool
isHigher p q = semitoneCount p > semitoneCount q

isLower :: Pitch -> Pitch -> Bool
isLower p q = semitoneCount p < semitoneCount q

equivalent :: Pitch -> Pitch -> Bool
equivalent p q = semitoneCount p == semitoneCount q


zequivalent :: PitchSpelling -> PitchSpelling -> Bool
zequivalent p q = zsemitoneCount p == zsemitoneCount q

arithmeticDistance :: Pitch -> Pitch -> Int
arithmeticDistance p q 
    | p `isHigher` q    = ad1 q p
    | otherwise         = ad1 p q
  where
    ad1 a@(Pitch s1 _) b@(Pitch s2 _) = 
        octaveDistance a b + zarithmeticDistance s1 s2
                       

zarithmeticDistance :: PitchSpelling -> PitchSpelling -> Int
zarithmeticDistance (PitchSpelling l1 _) (PitchSpelling l2 _) = 
    arithmeticDistancePL l1 l2

arithmeticDistancePL :: PitchLetter -> PitchLetter -> Int
arithmeticDistancePL start end = step (fromEnum start) (fromEnum end)
  where
    step i j | i < j            = 1 + (j-i)
             | otherwise        = 1 + (i-j)


semitoneDistance :: Pitch -> Pitch -> Int
semitoneDistance p q 
    | p `isHigher` q    = sd1 q p
    | otherwise         = sd1 p q
  where
    sd1 a b = semitoneCount b - semitoneCount a


semitonesToNext :: PitchSpelling -> PitchSpelling -> Int
semitonesToNext s1 s2
    | s1 == s2  = 12
    | otherwise = semitoneDistance p q
  where
    p = Pitch s1 1
    q = let q0 = Pitch s2 1 in if q0 `isLower` p then Pitch s2 2 else q0

-- | Note - returns a positive number.
semitonesToPrev :: PitchSpelling -> PitchSpelling -> Int
semitonesToPrev s1 s2 
    | s1 == s2  = 12 
    | otherwise = 12 - semitonesToNext s1 s2


-- | Note - should always be positive.
--
octaveDistance :: Pitch -> Pitch -> Int
octaveDistance p q
    | p `isHigher` q    = fn q p
    | otherwise         = fn p q
  where
    fn a b = let x = semitoneCount (naturalOf b) - semitoneCount (naturalOf a)
             in x `div` 12

--------------------------------------------------------------------------------
-- Interval


data IntervalType = OCTAVE | SECOND | THIRD | FOURTH | FIFTH | SIXTH | SEVENTH


-- | Note - intervals should be unsigned. If either of the
-- component parts is a negative number the Interval is 
-- ill-formed.
--
data Interval = Interval
    { interval_arith_dist       :: !Int
    , interval_semitones        :: !Int
    }
  deriving (Data,Eq,Ord,Show,Typeable)


intervalBetween :: Pitch -> Pitch -> Interval
intervalBetween p q 
    | p `isHigher` q    = fn q p
    | otherwise         = fn p q
  where
    fn a b = Interval { interval_arith_dist = arithmeticDistance a b
                      , interval_semitones  = semitoneCount b - semitoneCount a
                      }

--------------------------------------------------------------------------------
-- Adding intervals to pitches

addInterval :: Pitch -> Interval -> Pitch
addInterval (Pitch ss o) iv = Pitch ss1 ov1
  where
    ss1   = pachetAdd ss iv
    ostep = if crossesTwelve ss iv then 1 else 0
    ov1   = o + ostep + octaveCount iv


subInterval :: Pitch -> Interval -> Pitch
subInterval (Pitch ss o) iv = Pitch ss1 ov1
  where
    ss1   = pachetSub ss iv
    ostep = if crossesZero ss iv then -1 else 0
    ov1   = o + ostep - octaveCount iv



-- | The algorith provided by Francois Pachet in 
--   An Object-Oriented Representation of Pitch-Classes, 
--   Intervals, Scales and Chords: The basic MusES
-- does not account for octaves:
--
pachetAdd :: PitchSpelling -> Interval -> PitchSpelling
pachetAdd sp0 (Interval { interval_arith_dist = ad
                        , interval_semitones = sc }) = PitchSpelling l1 alt
  where
    (PitchSpelling l _)         = znaturalOf sp0
    znext@(PitchSpelling l1 _)  = PitchSpelling (upwardPL l ad) NAT
    sc_next                     = semitonesToNext sp0 znext
    alt                         = alterationFromDiff $ sc - sc_next


pachetSub :: PitchSpelling -> Interval -> PitchSpelling
pachetSub sp0 (Interval { interval_arith_dist = ad
                        , interval_semitones = sc }) = PitchSpelling l1 alt
  where
    (PitchSpelling l _)         = znaturalOf sp0
    zprev@(PitchSpelling l1 _)  = PitchSpelling (downwardPL l ad) NAT
    sc_prev                     = semitonesToPrev sp0 zprev
    alt                         = alterationFromDiff $ sc - sc_prev



-- | Step 2 in Pachet
upwardPL :: PitchLetter -> Int -> PitchLetter
upwardPL l ad = let n = fromEnum l in toEnum $ (n + (ad - 1)) `mod` 7

downwardPL :: PitchLetter -> Int -> PitchLetter
downwardPL l ad = let n = fromEnum l in toEnum $ (n - (ad - 1)) `mod` 7


alterationFromDiff :: Int -> Alteration
alterationFromDiff i 
    | i >= 2    = DBL_SHARP
    | i == 1    = SHARP
    | i == 0    = NAT
    | i == (-1) = FLAT
    | otherwise = DBL_FLAT


-- | Does the addition of the interval result in a /crossing/ to 
-- the next octave?
-- e.g.
--
--   > c(sc=0) `upto` f(sc=5) does not cross
--   > f(sc=5) `upto` c(sc=0) crosses
-- 
-- Crossing here is (>=).
--
crossesTwelve :: PitchSpelling -> Interval -> Bool
crossesTwelve ps iv = scount >= 12
  where
    scount = zsemitoneCount ps + interval_semitones (simpleIntervalOf iv)
   

-- | Does the subtracion of the interval result in a /crossing/ to 
-- the previous octave?
-- e.g.
--
--   > c(sc=0) `downto` f(sc=5) crosses
--   > f(sc=5) `downto` c(sc=0) does not cross
-- 
-- Crossing here is (>=).
--
crossesZero :: PitchSpelling -> Interval -> Bool
crossesZero ps iv = scount < 0
  where
    scount = zsemitoneCount ps - interval_semitones (simpleIntervalOf iv)
   

--------------------------------------------------------------------------------
-- More operations

isSmaller :: Interval -> Interval -> Bool
isSmaller i1 i2 = interval_semitones i1 < interval_semitones i2

isLarger :: Interval -> Interval -> Bool
isLarger i1 i2 = interval_semitones i1 > interval_semitones i2


octaveCount :: Interval -> Int
octaveCount (Interval { interval_semitones = n }) = n `div` 12

addOctaves :: Interval -> Int -> Interval
addOctaves (Interval { interval_arith_dist = ad
                     , interval_semitones  = sc }) i = 
    Interval { interval_arith_dist = ad + (i * 8)
             , interval_semitones  = sc + (i * 12)
             }


arithDistModulo :: Int -> Int
arithDistModulo ad = let x = ad-1 in 1+(x `mod` 7)


perfect_octave :: Interval
perfect_octave = Interval { interval_arith_dist = 8
                          , interval_semitones  = 12    
                          }

-- | Simple intervals are smaller intervals than perfect_octave
-- 
simpleIntervalOf :: Interval -> Interval
simpleIntervalOf iv@(Interval { interval_arith_dist = ad
                              , interval_semitones  = n  }) 
    | iv `isSmaller` perfect_octave  = iv
    | otherwise                      = Interval { interval_arith_dist = ad1
                                                , interval_semitones  = n1 }
  where
    ad1 = arithDistModulo ad
    n1  = n `mod` 12


invertSimpleInterval :: Interval -> Interval
invertSimpleInterval = step . simpleIntervalOf 
  where
    step (Interval { interval_arith_dist = ad, interval_semitones = n }) = 
        Interval { interval_arith_dist  = 9 - ad, interval_semitones   = 12 - n }



--------------------------------------------------------------------------------
-- Description

description :: Interval -> (String,String,Int)
description iv = (intervalColour iv, distanceName iv, octaveCount iv)



distanceName :: Interval -> String
distanceName (Interval {interval_arith_dist = ad}) 
    | ad == 1   = "unison"
    | otherwise = step $ arithDistModulo ad
  where
    step 1 = "octave"
    step 2 = "second"
    step 3 = "third"
    step 4 = "fourth"
    step 5 = "fifth"
    step 6 = "sixth"
    step 7 = "seventh"
    step _ = "distanceName - unreachable"

intervalColour :: Interval -> String
intervalColour (Interval {interval_arith_dist = ad, interval_semitones = n}) = 
    maybe "unknown" id $ case arithDistModulo ad of
      1 -> identify ["perfect", "augmented"] nmod
      2 -> identify ["diminished", "minor", "major", "augmented"] nmod
      3 -> identify ["diminished", "minor", "major", "augmented"] (nmod-2)
      4 -> identify ["diminished", "perfect", "augmented"] (nmod-4)
      5 -> identify ["diminished", "perfect", "augmented"] (nmod-6)
      6 -> identify ["diminished", "minor", "major", "augmented"] (nmod-7)
      7 -> identify ["diminished", "minor", "major", "augmented"] (nmod-9)
      _ -> Nothing
  where
    nmod     = n `mod` 12
    identify :: [String] -> Int -> Maybe String
    identify names ix | ix >= length names = Nothing
                      | otherwise          = Just $ names !! ix


