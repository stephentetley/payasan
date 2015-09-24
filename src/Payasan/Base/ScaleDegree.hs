{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.ScaleDegree
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Alternative diatonic pitch representation.
-- 
-- Simplifies diatonic transposition, (diatonic) inversion...
--
--------------------------------------------------------------------------------

module Payasan.Base.ScaleDegree
   

   where


import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Pitch

import Data.Data

--------------------------------------------------------------------------------
-- Syntax

data Degree = TONIC | SUPERTONIC | MEDIANT | SUBDOMINANT 
            | DOMINANT | SUBMEDIANT | LEADING_TONE
  deriving (Data,Enum,Eq,Ord,Show,Typeable)


data ScaleDegree = ScaleDegree Degree Alt 
  deriving (Data,Eq,Show,Typeable)


newtype Alt = Alt Int
  deriving (Data,Eq,Num,Ord,Show,Typeable)

-- | Notion of ord is complicated here, it depends on what octave 
-- means...
--
data OveScaleDegree = OveScaleDegree ScaleDegree Octave
  deriving (Data,Eq,Show,Typeable)




toPitch :: Key -> OveScaleDegree -> Pitch
toPitch _key (OveScaleDegree deg o) = Pitch name o
  where
    name = undefined


fromPitch :: Key -> Pitch -> OveScaleDegree
fromPitch _key (Pitch name o) = OveScaleDegree deg o
  where
    deg = undefined






fromPitchName :: Key -> PitchName -> ScaleDegree
fromPitchName key (PitchName letter alt) = 
    let deg      = letterToDegree key letter
        root_alt = findRootAlt key deg
    in ScaleDegree deg (alterationBetween root_alt (toAlt alt))




toAlt :: Alteration -> Alt
toAlt DBL_FLAT  = -2
toAlt FLAT      = -1
toAlt NAT       = 0
toAlt SHARP     = 1
toAlt DBL_SHARP = 2



findRootAlt :: Key -> Degree -> Alt
findRootAlt _cmaj TONIC        = 0
findRootAlt _cmaj SUPERTONIC   = 0
findRootAlt _cmaj MEDIANT      = 0
findRootAlt _cmaj SUBDOMINANT  = 0
findRootAlt _cmaj DOMINANT     = 0
findRootAlt _cmaj SUBMEDIANT   = 0
findRootAlt _cmaj LEADING_TONE = 0






modShift1 :: Integral a => a -> a -> a
modShift1 x m = 1 + ((x-1) `mod` m)



-- | Works
--
letterToDegree :: Key -> PitchLetter -> Degree
letterToDegree (Key (PitchName kletter _) _) letter = 
    intToDegree $ (1 + letterToIntCmaj letter - diff) `modShift1` 7
  where
    diff :: Int
    diff = letterToIntCmaj kletter



letterToDegreeCmaj :: PitchLetter -> Degree
letterToDegreeCmaj C = TONIC
letterToDegreeCmaj D = SUPERTONIC
letterToDegreeCmaj E = MEDIANT
letterToDegreeCmaj F = SUBDOMINANT
letterToDegreeCmaj G = DOMINANT
letterToDegreeCmaj A = SUBMEDIANT
letterToDegreeCmaj B = LEADING_TONE

letterToIntCmaj :: PitchLetter -> Int
letterToIntCmaj C = 1
letterToIntCmaj D = 2
letterToIntCmaj E = 3
letterToIntCmaj F = 4
letterToIntCmaj G = 5
letterToIntCmaj A = 6
letterToIntCmaj B = 7

degreeToInt :: Degree -> Int
degreeToInt TONIC        = 1
degreeToInt SUPERTONIC   = 2
degreeToInt MEDIANT      = 3
degreeToInt SUBDOMINANT  = 4
degreeToInt DOMINANT     = 5
degreeToInt SUBMEDIANT   = 6
degreeToInt LEADING_TONE = 7


intToDegree :: Int -> Degree
intToDegree i = step $ i `modShift1` 7
  where
    step 1  = TONIC 
    step 2  = SUPERTONIC 
    step 3  = MEDIANT 
    step 4  = SUBDOMINANT 
    step 5  = DOMINANT
    step 6  = SUBMEDIANT 
    step 7  = LEADING_TONE
    step n  = error $ "toDegree - unreachable: " ++ show n

alterationBetween :: Alt -> Alt -> Alt
alterationBetween a1 a2 = a2 - a1



data IntervalType = UNISON | SECOND | THIRD | FOURTH | FIFTH | SIXTH | SEVENTH | OCTAVE
  deriving (Data,Eq,Ord,Show,Typeable)
