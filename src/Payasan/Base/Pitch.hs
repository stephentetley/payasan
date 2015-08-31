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
--------------------------------------------------------------------------------

module Payasan.Base.Pitch
  ( 
    Pitch(..)
  , NoteLabel(..)
  , PitchLetter(..)
  , Alteration(..)
  , Octave

  , middle_c

  , semitoneCountNL
  , semitoneCountPL
  , semitoneCountA

  , extractLabel 

  , nearest

  )
  where


import Data.Data


-- | middle c is c4
data Pitch = Pitch !NoteLabel !Octave 
  deriving (Data,Eq,Ord,Show,Typeable)

data NoteLabel = NoteLabel !PitchLetter !Alteration
  deriving (Data,Eq,Ord,Show,Typeable)


data PitchLetter = C | D | E | F | G | A | B 
  deriving (Bounded,Data,Enum,Eq,Ord,Show,Typeable)

data Alteration = DBL_FLAT | FLAT | NAT | SHARP | DBL_SHARP
  deriving (Data,Enum,Eq,Ord,Show,Typeable)


type Octave = Int

middle_c :: Pitch
middle_c = Pitch (NoteLabel C NAT) 4

semitoneCountNL :: NoteLabel -> Int
semitoneCountNL (NoteLabel pl a) = semitoneCountPL pl + semitoneCountA a


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


extractLabel :: Pitch -> NoteLabel
extractLabel (Pitch lbl _) = lbl


-- Need this fot LilyPond.
nearest :: NoteLabel -> Pitch -> Pitch
nearest _lbl _pch = error "Pitch.nearest"


{-
equivalent :: NoteLabel -> NoteLabel -> Bool
equivalent n1 n2 = semitoneCountN n1 == semitoneCountN n2


sharpA :: Alteration -> Alteration
sharpA DBL_FLAT  = FLAT
sharpA FLAT      = NAT
sharpA NAT       = SHARP
sharpA SHARP     = DBL_SHARP
sharpA DBL_SHARP = error "sharpA - DBL_SHARP"


sharp :: NoteLabel -> NoteLabel
sharp (NoteLabel pl a) = NoteLabel pl $ sharpA a

flatA :: Alteration -> Alteration
flatA DBL_FLAT  = error "flatA - DBL_FLAT"
flatA FLAT      = DBL_FLAT
flatA NAT       = FLAT
flatA SHARP     = NAT
flatA DBL_SHARP = SHARP

flat :: NoteLabel -> NoteLabel 
flat (NoteLabel pl a) = NoteLabel pl $ flatA a


--------------------------------------------------------------------------------
-- Interval


data IntervalType = OCTAVE | SECOND | THIRD | FOURTH | FIFTH | SIXTH | SEVENTH

data Interval = Interval
    { interval_type             :: !Int
    , interval_semicount        :: !Int
    }
  deriving (Data,Eq,Ord,Show,Typeable)


-- constructor examples

fifth                   :: Interval
fifth                   = Interval { interval_type        = 5
                                   , interval_semicount   = 7
                                   }

diminishedSeventh       :: Interval
diminishedSeventh       = Interval { interval_type        = 7
                                   , interval_semicount   = 9
                                   }



intervalName :: Interval -> String
intervalName ivl = unwords [ distanceName ivl, intervalColour ivl ]

identify :: [String] -> Int -> Maybe String
identify names ix | ix >= length names = Nothing
                  | otherwise          = Just $ names !! ix

distanceName :: Interval -> String
distanceName (Interval {interval_type = ty}) = step $ (ty-1) `mod` 7
  where
    step 0 = "octave"
    step 1 = "second"
    step 2 = "third"
    step 3 = "fourth"
    step 4 = "fifth"
    step 5 = "sixth"
    step 6 = "seventh"
    step _ = error $ "distanceName - unreachable"

intervalColour :: Interval -> String
intervalColour (Interval {interval_type = ty, interval_semicount = n}) = 
    maybe "unknown colour" id $ case ty of
      2 -> identify ["diminished", "minor", "major", "augmented"] (n+1)
      3 -> identify ["minor", "major"] (n-2)
      4 -> identify ["diminished", "perfect", "augmented"] (n-3)
      5 -> identify ["diminished", "perfect", "augmented"] (n-5)
      6 -> identify ["minor", "major", "augmented"] (n-7)
      7 -> identify ["diminished", "minor", "major"] (n-8)
      _ -> Nothing

reverseInterval :: Interval -> Interval
reverseInterval (Interval {interval_type = ty, interval_semicount = n}) = 
    Interval { interval_type        = 9 - ty
             , interval_semicount   = 12 - n
             }

-}