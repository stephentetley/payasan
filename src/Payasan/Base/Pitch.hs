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
-- z- prefix on functions indicates they operate on PitchName
-- which has no notion of octave (c.f. Z12 modulo representation).
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


  , nextPitchLetter
  , prevPitchLetter
  , nthPitchLetterUp
  , nthPitchLetterDown
  , arithmeticNaturalUp
  , arithmeticNaturalDown

  , naturalOf
  , znaturalOf
  , sharpOf
  , zsharpOf
  , flatOf
  , zflatOf

  , fromAlteration
  , toAlteration
  , semitoneCount
  , midiSemitoneCount
  , zsemitoneCount
  , zequivalent

  
  , arithmeticDistance
  , zarithmeticDistanceUp
  , zarithmeticDistanceDown

  , semitoneDistance

  , octaveDistance
  , lyOctaveDistance

  , nearestRootToC4

  -- * Intervals
  , Interval(..)
  , intervalPlus
  , (^+^)

  , makeCompound
  , intervalBetween
  , addInterval
  , subInterval

  , (.+^) 
  , (.-^)

  , isSmaller
  , isLarger
  , octaveCount
  , addOctaves
  , arithDistModulo

  , simpleIntervalOf

  , invertSimpleInterval

  , intervalDescription

  )
  where

import Payasan.Base.Internal.Base

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


nextPitchLetter :: PitchLetter -> PitchLetter
nextPitchLetter B = C
nextPitchLetter l = succ l

prevPitchLetter :: PitchLetter -> PitchLetter
prevPitchLetter C = B
prevPitchLetter l = pred l

-- | Counts from 0 (sub 1 initially for arithmetic steps up)
--
nthPitchLetterUp :: Int -> PitchLetter -> PitchLetter
nthPitchLetterUp n letter
    | n <  0    = nthPitchLetterDown (abs n) letter
    | otherwise = let n1 = n `mod` 7
                      n2 = (fromEnum letter + n1) `mod` 7
                  in toEnum n2
  
-- | Counts from 0 (sub 1 initially for arithmetic steps up)
--
nthPitchLetterDown :: Int -> PitchLetter -> PitchLetter
nthPitchLetterDown n sd 
    | n <  0    = nthPitchLetterUp (abs n) sd
    | otherwise = let n1 = n `mod` 7
                      n2 = (fromEnum sd - n1) `mod` 7
                  in toEnum n2


-- DESIGN NOTE
-- could use arithmeticNaturalUp/Down for addInterval...

-- | Int should be positive.
--
arithmeticNaturalUp :: Int -> Pitch -> Pitch
arithmeticNaturalUp n (Pitch name o) = 
    let (om,name2) = arithmeticNaturalUp1 n name in Pitch name2 (o + om)


arithmeticNaturalUp1 :: Int -> PitchName -> (Int,PitchName)
arithmeticNaturalUp1 n (PitchName letter _) = 
    (om + carry, PitchName letter2 NAT)
  where
    (om,pm) = (\(a,b) -> (a,b+1)) $ ((n-1) `divMod` 7)
    letter2 = nthPitchLetterUp (pm-1) letter
    carry   = if fromEnum letter2 < fromEnum letter then 1 else 0



-- | Int should be positive.
--
arithmeticNaturalDown :: Int -> Pitch -> Pitch
arithmeticNaturalDown n (Pitch name o) = 
    let (om,name2) = arithmeticNaturalDown1 n name in Pitch name2 (o - om)


-- | carry is positive (should be subtracted)
--
arithmeticNaturalDown1 :: Int -> PitchName -> (Int,PitchName)
arithmeticNaturalDown1 n (PitchName letter _) = 
    (om + carry, PitchName letter2 NAT)
  where
    (om,pm) = (\(a,b) -> (a,b+1)) $ ((n-1) `divMod` 7)
    letter2 = nthPitchLetterDown (pm-1) letter
    carry   = if fromEnum letter2 > fromEnum letter then 1 else 0



naturalOf :: Pitch -> Pitch
naturalOf (Pitch s o) = Pitch (znaturalOf s) o

znaturalOf :: PitchName -> PitchName
znaturalOf (PitchName l _) = PitchName l NAT

sharpOf :: Pitch -> Pitch
sharpOf (Pitch s o) = Pitch (zsharpOf s) o

zsharpOf :: PitchName -> PitchName
zsharpOf (PitchName l _) = PitchName l SHARP

flatOf :: Pitch -> Pitch
flatOf (Pitch s o) = Pitch (zflatOf s) o

zflatOf :: PitchName -> PitchName
zflatOf (PitchName l _) = PitchName l FLAT



fromAlteration :: Integral a => Alteration -> a
fromAlteration DBL_FLAT  = -2
fromAlteration FLAT      = -1
fromAlteration NAT       = 0
fromAlteration SHARP     = 1
fromAlteration DBL_SHARP = 2

toAlteration :: Integral a => a -> Alteration
toAlteration i 
    | i <= -2       = DBL_FLAT
    | i == -1       = FLAT
    | i ==  0       = NAT
    | i ==  1       = SHARP
    | otherwise     = DBL_SHARP

-- | Middle C is 48 - to get MIDI semitone count add 12
--
semitoneCount :: Pitch -> Int
semitoneCount (Pitch s o) = zsemitoneCount s + (12 * o)

-- | Middle C is 48 - to get MIDI semitone count add 12
--
midiSemitoneCount :: Pitch -> Int
midiSemitoneCount p = 12 + semitoneCount p



zsemitoneCount :: PitchName -> Int
zsemitoneCount (PitchName pl a) = semitoneCountPL pl + fromAlteration a


semitoneCountPL :: PitchLetter -> Int
semitoneCountPL C = 0
semitoneCountPL D = 2
semitoneCountPL E = 4
semitoneCountPL F = 5
semitoneCountPL G = 7
semitoneCountPL A = 9
semitoneCountPL B = 11

instance PitchOrd Pitch where
  equivalent p q = semitoneCount p == semitoneCount q
  isHigher   p q = semitoneCount p >  semitoneCount q
  isLower    p q = semitoneCount p <  semitoneCount q



zequivalent :: PitchName -> PitchName -> Bool
zequivalent p q = zsemitoneCount p == zsemitoneCount q


arithmeticDistance :: Pitch -> Pitch -> Int
arithmeticDistance p q 
    | p == q            = 1
    | q `isHigher` p    = adUp p q
    | otherwise         = adDown p q
  where
    adUp   a@(Pitch s1 _) b@(Pitch s2 _) = 
        (7 * octaveDistance a b) + zarithmeticDistanceUp s1 s2

    adDown a@(Pitch s1 _) b@(Pitch s2 _) = 
       (7 * octaveDistance b a) + zarithmeticDistanceDown s1 s2


zarithmeticDistanceUp :: PitchName -> PitchName -> Int
zarithmeticDistanceUp (PitchName l1 _) (PitchName l2 _) = 
    adUpwardsPL l1 l2

zarithmeticDistanceDown :: PitchName -> PitchName -> Int
zarithmeticDistanceDown (PitchName l1 _) (PitchName l2 _) = 
    adDownwardsPL l1 l2


adUpwardsPL :: PitchLetter -> PitchLetter -> Int
adUpwardsPL start end = step (fromEnum start) (fromEnum end)
  where
    step i j | i < j            = 1 + (j-i)
             | otherwise        = let hij = j+7 in 1 + (hij-i)

adDownwardsPL :: PitchLetter -> PitchLetter -> Int
adDownwardsPL start end = step (fromEnum start) (fromEnum end)
  where
    step i j | i > j            = 1 + (i-j)
             | otherwise        = let hii = i+7 in 1 + (hii-j)


semitoneDistance :: Pitch -> Pitch -> Int
semitoneDistance p q 
    | p `isHigher` q    = sd1 q p
    | otherwise         = sd1 p q
  where
    sd1 a b = semitoneCount b - semitoneCount a



-- | Note - should always be positive.
--
octaveDistance :: Pitch -> Pitch -> Int
octaveDistance p q
    | p `isHigher` q    = fn q p
    | otherwise         = fn p q
  where
    fn a b = let x = semitoneCount (naturalOf b) - semitoneCount (naturalOf a)
             in x `div` 12

lyOctaveDistance :: Pitch -> Pitch -> Int
lyOctaveDistance root p1 
    | root == p1         = 0
    | root `isHigher` p1 = negate $ fn p1 root
    | otherwise          = fn root p1
  where
    fn a b = let ad     = arithmeticDistance a b 
                 ostep  = if ad >= 5 then 1 else 0
                 ove    = octaveDistance a b
             in ostep + ove





nearestRootToC4 :: PitchName -> Pitch
nearestRootToC4 name = if addown < adup then pdown else psame
  where
    psame  = Pitch name 4
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


intervalBetween :: Pitch -> Pitch -> Interval
intervalBetween p q 
    | p `isHigher` q    = fn q p
    | otherwise         = fn p q
  where
    fn a b = let sc = semitoneCount b - semitoneCount a
             in Interval { interval_distance  = arithmeticDistance a b
                         , interval_semitones = sc 
                         }


makeCompound :: Int -> Interval -> Interval
makeCompound i ivl = fn $ simpleIntervalOf ivl
  where
    fn (Interval { interval_distance  = d
                 , interval_semitones = sc }) = 
        let d1 = d - 1 + 8 * i
            sc1 = sc + 12 * i
        in Interval { interval_distance  = d1, interval_semitones = sc1 }

--------------------------------------------------------------------------------
-- Adding intervals to pitches

addInterval :: Pitch -> Interval -> Pitch
addInterval (Pitch ss o) iv = Pitch ss1 ov1
  where
    ss1   = pachetAdd ss iv
    ostep = if crossesTwelve ss iv then 1 else 0
    ov1   = o + ostep + octaveCount iv



infixl 6 .+^

-- | Alias for addInterval. Name and fixity follows vector-space
-- library but we don\'t depend on it.
--
-- (This may change.)
--
(.+^) :: Pitch -> Interval -> Pitch
(.+^) = addInterval

subInterval :: Pitch -> Interval -> Pitch
subInterval (Pitch ss o) iv = Pitch ss1 ov1
  where
    ss1   = pachetSub ss iv
    ostep = if crossesZero ss iv then -1 else 0
    ov1   = o + ostep - octaveCount iv


infixl 6 .-^

-- | Alias for subInterval. Name and fixity follows vector-space
-- library but we don\'t depend on it.
--
-- (This may change.)
--
(.-^) :: Pitch -> Interval -> Pitch
(.-^) = subInterval


-- | The algorith provided by Francois Pachet in 
--   An Object-Oriented Representation of Pitch-Classes, 
--   Intervals, Scales and Chords: The basic MusES
-- does not account for octaves:
--
pachetAdd :: PitchName -> Interval -> PitchName
pachetAdd sp0 (Interval { interval_distance  = ad
                        , interval_semitones = sc }) = PitchName l1 alt
  where
    (PitchName l0 _)            = znaturalOf sp0
    znext@(PitchName l1 _)      = PitchName (upwardPL l0 ad) NAT
    sc_next                     = semitonesToNext sp0 znext
    alt                         = alterationFromDiff $ sc - sc_next


pachetSub :: PitchName -> Interval -> PitchName
pachetSub sp0 (Interval { interval_distance = ad
                        , interval_semitones = sc }) = PitchName l1 alt
  where
    (PitchName l0 _)            = znaturalOf sp0
    zprev@(PitchName l1 _)      = PitchName (downwardPL l0 ad) NAT
    sc_prev                     = semitonesToPrev sp0 zprev
    alt                         = alterationFromDiff $ sc - sc_prev



-- | Step 2 in Pachet
upwardPL :: PitchLetter -> Int -> PitchLetter
upwardPL l ad = let n = fromEnum l in toEnum $ (n + (ad - 1)) `mod` 7

downwardPL :: PitchLetter -> Int -> PitchLetter
downwardPL l ad = let n = fromEnum l in toEnum $ (n - (ad - 1)) `mod` 7


-- | Step 3 in Pachet

semitonesToNext :: PitchName -> PitchName -> Int
semitonesToNext s1 s2
    | s1 == s2  = 0
    | otherwise = semitoneDistance p q
  where
    p = Pitch s1 1
    q = let q0 = Pitch s2 1 in if q0 `isLower` p then Pitch s2 2 else q0

-- | Note - returns a positive number.
semitonesToPrev :: PitchName -> PitchName -> Int
semitonesToPrev s1 s2 
    | s1 == s2  = 0
    | otherwise = 12 - semitonesToNext s1 s2



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
crossesTwelve :: PitchName -> Interval -> Bool
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
crossesZero :: PitchName -> Interval -> Bool
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
addOctaves (Interval { interval_distance = ad
                     , interval_semitones  = sc }) i = 
    Interval { interval_distance = ad + (i * 8)
             , interval_semitones  = sc + (i * 12)
             }


arithDistModulo :: Int -> Int
arithDistModulo ad = let x = ad-1 in 1+(x `mod` 7)


perfect_octave :: Interval
perfect_octave = Interval { interval_distance = 8
                          , interval_semitones  = 12    
                          }

-- | Simple intervals are smaller intervals than perfect_octave
-- 
simpleIntervalOf :: Interval -> Interval
simpleIntervalOf iv@(Interval { interval_distance = ad
                              , interval_semitones  = n  }) 
    | iv `isSmaller` perfect_octave  = iv
    | otherwise                      = Interval { interval_distance = ad1
                                                , interval_semitones  = n1 }
  where
    ad1 = arithDistModulo ad
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
intervalColour (Interval {interval_distance = ad, interval_semitones = n}) = 
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


