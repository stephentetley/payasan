{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Pitch
-- Copyright   :  (c) Stephen Tetley 2015-2016
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
--------------------------------------------------------------------------------

module Payasan.Base.Pitch
  ( 

  -- * Pitch
    Pitch(..)
  , PitchName(..)
  , PitchLetter(..)
  , Alteration(..)
  , Octave

  , middle_c
  , c_nat

  , isNatural
  , isAltered 
  , setAlteration
  , asNatural
  , asSharp
  , asFlat

  , toAlteration
  , fromAlteration
  , fromPitchLetter


  , semitoneCount
  , midiSemitoneCount

  , arithmeticDistance
  , semitoneDistance

  , lyOctaveDistance
  , nearestRootToC4

  , Natural
  , toNatural
  , fromNatural
  , fromPosition
  , toPosition
  , naturalAddArithDist
  , naturalSubArithDist


  -- * Intervals
  , Interval(..)
  , isSmaller
  , isLarger
  , octaveCount

  , intervalBetween

  , intervalPlus
  , (^+^)

  , addInterval
  , (.+^) 

  , subInterval
  , (.-^)

  , simpleIntervalOf
  , invertSimpleInterval


  , intervalDescription

  )
  where

import Payasan.Base.Basis
import Payasan.Base.Utils

import Text.PrettyPrint.HughesPJClass           -- package: pretty

import Data.Data


-- | middle c is c4
data Pitch = Pitch 
    { pitch_name        :: !PitchName
    , pitch_octave      :: !Octave 
    }
  deriving (Data,Eq,Ord,Show,Typeable)

data PitchName = PitchName 
    { pitch_letter      :: !PitchLetter
    , pitch_alteration  :: !Alteration
    }
  deriving (Data,Eq,Ord,Show,Typeable)


data PitchLetter = C | D | E | F | G | A | B 
  deriving (Bounded,Data,Enum,Eq,Ord,Show,Typeable)

data Alteration = DBL_FLAT | FLAT | NAT | SHARP | DBL_SHARP
  deriving (Bounded,Data,Enum,Eq,Ord,Show,Typeable)


type Octave = Int

instance Pretty Pitch where
  pPrint (Pitch ss o)        = pPrint ss <> pPrint o

instance Pretty PitchName where
  pPrint (PitchName l a) = pPrint l <> pPrint a

instance Pretty PitchLetter where
  pPrint C              = char 'C'
  pPrint D              = char 'D'
  pPrint E              = char 'E'
  pPrint F              = char 'F'
  pPrint G              = char 'G'
  pPrint A              = char 'A'
  pPrint B              = char 'B'

instance Pretty Alteration where
  pPrint DBL_FLAT       = text "bb"
  pPrint FLAT           = char 'b'
  pPrint NAT            = empty
  pPrint SHARP          = char '#'
  pPrint DBL_SHARP      = text "##"

-- | Middle C is octave 4 as per /scientific notation/.
--
middle_c        :: Pitch
middle_c        = Pitch c_nat 4

c_nat           :: PitchName 
c_nat           = PitchName C NAT


--------------------------------------------------------------------------------
-- Operations

isNatural :: Pitch -> Bool
isNatural (Pitch (PitchName _ NAT) _) = True
isNatural _                           = False

isAltered :: Pitch -> Bool
isAltered (Pitch (PitchName _ NAT) _) = False
isAltered _                           = True



setAlteration :: Pitch -> Alteration -> Pitch
setAlteration (Pitch (PitchName l _) o) a = Pitch (PitchName l a) o

asNatural       :: Pitch -> Pitch
asNatural p     = setAlteration p NAT

asSharp         :: Pitch -> Pitch
asSharp p       = setAlteration p SHARP

asFlat          :: Pitch -> Pitch
asFlat p        = setAlteration p FLAT


toAlteration :: Integral a => a -> Alteration
toAlteration i 
    | i <= -2       = DBL_FLAT
    | i == -1       = FLAT
    | i ==  0       = NAT
    | i ==  1       = SHARP
    | otherwise     = DBL_SHARP

fromAlteration :: Integral a => Alteration -> a
fromAlteration DBL_FLAT  = -2
fromAlteration FLAT      = -1
fromAlteration NAT       = 0
fromAlteration SHARP     = 1
fromAlteration DBL_SHARP = 2


fromPitchLetter :: PitchLetter -> Int
fromPitchLetter C       = 0
fromPitchLetter D       = 2
fromPitchLetter E       = 4
fromPitchLetter F       = 5
fromPitchLetter G       = 7
fromPitchLetter A       = 9
fromPitchLetter B       = 11


-- | Middle C is 48 - to get MIDI semitone count add 12
--
semitoneCount :: Pitch -> Int
semitoneCount (Pitch (PitchName l a) o) = 
    fromPitchLetter l + fromAlteration a + 12 * o





-- | Middle C is 48 - to get MIDI semitone count add 12
--
midiSemitoneCount :: Pitch -> Int
midiSemitoneCount p = 12 + semitoneCount p



instance PitchOrd Pitch where
  equivalent p q = semitoneCount p == semitoneCount q
  isHigher   p q = semitoneCount p >  semitoneCount q
  isLower    p q = semitoneCount p <  semitoneCount q


--------------------------------------------------------------------------------
-- Natural - simplifies arithmetic distance calculations


data Natural = Natural
    { natural_letter    :: !PitchLetter
    , natural_octave    :: !Octave 
    }
  deriving (Data,Eq,Show,Typeable)


toNatural :: Pitch -> Natural
toNatural (Pitch (PitchName l _) o) = Natural { natural_letter = l
                                              , natural_octave = o }


fromNatural :: Natural -> Pitch
fromNatural (Natural l o) = Pitch (PitchName l NAT) o

semitones :: Natural -> Int
semitones (Natural l o) = fromPitchLetter l + 12 * o

toPosition :: Natural -> Int
toPosition (Natural l o) = fromEnum l + 7 * o

fromPosition :: Int -> Natural 
fromPosition n = let (o,i) = n `divMod` 7 in Natural (toEnum i) o

instance PitchOrd Natural where
  equivalent p q = semitones p == semitones q
  isHigher   p q = semitones p >  semitones q
  isLower    p q = semitones p <  semitones q


-- | Always positive.
--
naturalDistance :: Natural -> Natural -> Int
naturalDistance p q 
    | p == q            = 1
    | q `isHigher` p    = distUp p q
    | otherwise         = distUp q p
  where
    distUp a b = 1 + toPosition b - toPosition a

-- | Always positive.
--
arithmeticDistance :: Pitch -> Pitch -> Int
arithmeticDistance p q = naturalDistance (toNatural p) (toNatural q)


semitoneDistance :: Pitch -> Pitch -> Int
semitoneDistance p q 
    | p `isHigher` q    = dist q p
    | otherwise         = dist p q
  where
    dist lo hi = semitoneCount hi - semitoneCount lo



naturalAddArithDist :: Natural -> Int -> Natural
naturalAddArithDist nat n = fromPosition $ toPosition nat + (n-1)


naturalSubArithDist :: Natural -> Int -> Natural
naturalSubArithDist nat n = fromPosition $ toPosition nat - (n+1)



--------------------------------------------------------------------------------
-- 

lyOctaveDistance :: Pitch -> Pitch -> Int
lyOctaveDistance root p1 = sign $ dist (toNatural root) (toNatural p1)
  where
    sign        = if root `isHigher` p1 then negate else id

    -- ad is always positive
    dist r n1   = let ad    = naturalDistance r n1
                      ostep = if ad >= 5 then 1 else 0
                      ove   = max 0 $ (ad-5) `div` 7
                  in ove + ostep



nearestRootToC4 :: PitchName -> Pitch
nearestRootToC4 name = if addown < adup then pdown else psame
  where
    psame   = Pitch name 4
    pdown   = Pitch name 3
    adup    = arithmeticDistance middle_c psame
    addown  = arithmeticDistance pdown middle_c


--------------------------------------------------------------------------------
-- Interval


-- | Note - intervals should be unsigned. If either of the
-- component parts is a negative number the Interval is 
-- ill-formed.
--
data Interval = Interval
    { interval_distance         :: !Int
    , interval_semitones        :: !Int
    }
  deriving (Data,Eq,Ord,Show,Typeable)



perfect_octave :: Interval
perfect_octave = Interval { interval_distance = 8
                          , interval_semitones  = 12    
                          }



isSmaller :: Interval -> Interval -> Bool
isSmaller i1 i2 = interval_semitones i1 < interval_semitones i2

isLarger :: Interval -> Interval -> Bool
isLarger i1 i2 = interval_semitones i1 > interval_semitones i2


octaveCount :: Interval -> Int
octaveCount (Interval { interval_semitones = n }) = n `div` 12



-- | Note - Interval is always _positive_.
--
intervalBetween :: Pitch -> Pitch -> Interval
intervalBetween p q = Interval { interval_distance  = arithmeticDistance p q
                               , interval_semitones = semitoneDistance p q
                               }



intervalPlus :: Interval -> Interval -> Interval
intervalPlus a b = 
    let ad = (interval_distance a + interval_distance b) - 1
        sc = interval_semitones a + interval_semitones b
    in Interval { interval_distance = ad, interval_semitones = sc }


infixl 6 ^+^

-- | Alias for intervalPlus. Name and fixity follows vector-space
-- library but we don\'t depend on it.
--
-- (This may change.)
--
(^+^) :: Interval -> Interval -> Interval
(^+^) = intervalPlus




addInterval :: Pitch -> Interval -> Pitch
addInterval p iv = setAlteration p2 (toAlteration sdiff)
  where
    nat1  = toNatural p
    nat2  = naturalAddArithDist nat1 (interval_distance iv)
    p2    = fromNatural nat2
    sdiff = interval_semitones iv - semitoneDistance p p2


infixl 6 .+^

-- | Alias for addInterval. Name and fixity follows vector-space
-- library but we don\'t depend on it.
--
-- (This may change.)
--
(.+^) :: Pitch -> Interval -> Pitch
(.+^) = addInterval

    
subInterval :: Pitch -> Interval -> Pitch
subInterval p iv = setAlteration p2 (toAlteration sdiff)
  where
    nat1  = toNatural p
    nat2  = naturalSubArithDist nat1 (interval_distance iv)
    p2    = fromNatural nat2
    sdiff = semitoneDistance p p2 - interval_semitones iv




-- | Alias for subInterval. Name and fixity follows vector-space
-- library but we don\'t depend on it.
--
-- (This may change.)
--
(.-^) :: Pitch -> Interval -> Pitch
(.-^) = subInterval




-- | Simple intervals are smaller intervals than perfect_octave
-- 
simpleIntervalOf :: Interval -> Interval
simpleIntervalOf iv@(Interval { interval_distance = ad
                              , interval_semitones  = n  }) 
    | iv `isSmaller` perfect_octave  = iv
    | otherwise                      = Interval { interval_distance = ad1
                                                , interval_semitones  = n1 }
  where
    ad1 = ad `modS1` 7
    n1  = n `mod` 12




invertSimpleInterval :: Interval -> Interval
invertSimpleInterval = step . simpleIntervalOf 
  where
    step (Interval { interval_distance = ad, interval_semitones = n }) = 
        Interval { interval_distance    = 9 - ad
                 , interval_semitones   = 12 - n }



--------------------------------------------------------------------------------
-- Description

intervalDescription :: Interval -> (String,String,Int)
intervalDescription iv = (intervalColour iv, distanceName iv, octaveCount iv)



distanceName :: Interval -> String
distanceName (Interval {interval_distance = ad}) 
    | ad == 1   = "unison"
    | otherwise = step $ ad `modS1` 7
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
intervalColour (Interval {interval_distance = ad, interval_semitones = n}) = 
    maybe "unknown" id $ case ad `modS1` 7 of
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


