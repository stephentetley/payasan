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

  , extractSpelling
  , naturalOf
  , sharpOf
  , flatOf
  , semitoneCount
  , zsemitoneCount
  , higher
  , lower
  , equivalent
  , zequivalent

  
  , arithmeticDistance
  , zarithmeticDistance

  , octaveDistance

  -- * Intervals
  , IntervalType(..)
  , Interval(..)
  , interval
  , smaller
  , larger
  , octaveCount
  , addOctaves
  , arithDistModulo
  , addInterval

  , simpleIntervalOf

--  , reverseInterval

  , description

  )
  where


import Data.Data


-- | middle c is c4
data Pitch = Pitch !PitchSpelling !Octave 
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


extractSpelling :: Pitch -> PitchSpelling
extractSpelling (Pitch s _) = s


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

semitoneCount :: Pitch -> Int
semitoneCount (Pitch s o) = zsemitoneCount s + 12 * (o+1)


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


higher :: Pitch -> Pitch -> Bool
higher p q = semitoneCount p > semitoneCount q

lower :: Pitch -> Pitch -> Bool
lower p q = semitoneCount p < semitoneCount q

equivalent :: Pitch -> Pitch -> Bool
equivalent p q = semitoneCount p == semitoneCount q


zequivalent :: PitchSpelling -> PitchSpelling -> Bool
zequivalent p q = zsemitoneCount p == zsemitoneCount q

arithmeticDistance :: Pitch -> Pitch -> Int
arithmeticDistance p q 
    | p `higher` q      = ad1 q p
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

-- | Note - should always be positive.
--
octaveDistance :: Pitch -> Pitch -> Int
octaveDistance p q
    | p `higher` q      = fn q p
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


interval :: Pitch -> Pitch -> Interval
interval p q 
    | p `higher` q      = fn q p
    | otherwise         = fn p q
  where
    fn a b = Interval { interval_arith_dist = arithmeticDistance a b
                      , interval_semitones  = semitoneCount b - semitoneCount a
                      }

smaller :: Interval -> Interval -> Bool
smaller i1 i2 = interval_semitones i1 < interval_semitones i2

larger :: Interval -> Interval -> Bool
larger i1 i2 = interval_semitones i1 > interval_semitones i2


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

addInterval :: Pitch -> Interval -> Pitch
addInterval _ _ = error "Pitch.addInterval"

perfect_unison :: Interval
perfect_unison = Interval { interval_arith_dist = 8
                          , interval_semitones  = 12    
                          }


simpleIntervalOf :: Interval -> Interval
simpleIntervalOf iv@(Interval { interval_arith_dist = ad
                              , interval_semitones  = n  }) 
    | iv `larger` perfect_unison = Interval { interval_arith_dist = ad1
                                            , interval_semitones  = n1 }
    | otherwise                   = iv
  where
    ad1 = let x0 = ad - 1 in 1 + (x0 `rem` 7)
    n1  = n `rem` 12


{-
-- TODO - this doesn't handle ove+ intervals
invertSimpleInterval :: Interval -> Interval
invertSimpleInterval (Interval { interval_arith_dist = ad
                               , interval_semitones  = n  }) = 
    Interval { interval_arith_dist  = 9 - ad
             , interval_semitones   = 12 - n
             }


-}



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


