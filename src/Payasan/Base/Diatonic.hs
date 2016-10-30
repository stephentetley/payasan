{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Diatonic
-- Copyright   :  (c) Stephen Tetley 2015-2016
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

module Payasan.Base.Diatonic

  (
    DiatonicScale
  , fromDiatonicScale
  , buildDiatonicScale

  , ScaleDegree(..)
  , Diatonic(..)

  , isTonic
  , isDiatonic
  , isChromatic
  , isChromaticSharp
  , isChromaticFlat


  , toDiatonic
  , fromDiatonic
  , nubAlteration

  , toScaleDegree
  , fromScaleDegree

  , scaleDegreeInt 

  , chromaticIndex
  , diatonicIndex 

  , SimpleInterval(..)
  , DiatonicInterval(..)

  , toDiatonicInterval
  , fromDiatonicInterval

  , diatonicIntervalBetween
  , addDiatonicInterval
  , subDiatonicInterval

  , transposeDiatonically


  ) where

import Payasan.Base.Scale
import Payasan.Base.Utils

import Payasan.Base.Pitch

import Text.PrettyPrint.HughesPJClass           -- package: pretty

import Data.Data
import qualified Data.Map as MAP


--------------------------------------------------------------------------------
-- Syntax

newtype DiatonicScale = DiatonicScale { 
    fromDiatonicScale :: MAP.Map ScaleDegree PitchName }
  deriving (Data,Eq,Show,Typeable)


-- buildDiatonicScale :: Key -> DiatonicScale
-- buildDiatonicScale key = DiatonicScale $ 
--     MAP.fromList $ zip [TONIC .. LEADING_TONE] (fromScale $ buildScale key)

buildDiatonicScale :: Scale -> DiatonicScale
buildDiatonicScale sc = DiatonicScale $ 
    MAP.fromList $ zip [TONIC .. LEADING_TONE] (fromScale sc)


-- | This throws an error for ill-formed scales.
-- 
lookupScaleDegree :: ScaleDegree -> DiatonicScale -> PitchName 
lookupScaleDegree ix (DiatonicScale scmap) = 
    case MAP.lookup ix scmap of
      Just p -> p
      Nothing -> error $ "findScaleDegree - ill-formed scale: " 
                       ++ show (MAP.elems scmap)



--------------------------------------------------------------------------------


data ScaleDegree = TONIC | SUPERTONIC | MEDIANT | SUBDOMINANT 
                 | DOMINANT | SUBMEDIANT | LEADING_TONE
  deriving (Bounded,Data,Enum,Eq,Ord,Show,Typeable)



data Diatonic = Diatonic 
    { dia_scale_degree  :: !ScaleDegree
    , dia_alt           :: !Alt
    , dia_octave        :: !Int
    }
  deriving (Data,Eq,Show,Typeable)


-- | Diatonic operations are oblivious to alterations
data DiaCore = DiaCore
    { dc_scale_degree :: !ScaleDegree
    , dc_octave       :: !Int
    }
  deriving (Data,Eq,Show,Typeable)


newtype Alt = Alt Int
  deriving (Data,Enum,Eq,Integral,Num,Ord,Real,Show,Typeable)


extractCore :: Diatonic -> DiaCore
extractCore (Diatonic { dia_scale_degree = sd, dia_octave = ove }) =
    DiaCore { dc_scale_degree = sd, dc_octave = ove }




succN :: Int -> ScaleDegree -> ScaleDegree
succN i sd | i < 0 = precN (abs i) sd
succN i sd         = toScaleDegree $ (fromScaleDegree sd) + i

precN :: Int -> ScaleDegree -> ScaleDegree
precN i sd | i < 0 = succN (abs i) sd
precN i sd         = toScaleDegree $ (fromScaleDegree sd) - i


isTonic :: Diatonic -> Bool
isTonic p = isDiatonic p && dia_scale_degree p == TONIC


isDiatonic :: Diatonic -> Bool
isDiatonic (Diatonic { dia_alt = a }) = a == 0

isChromatic :: Diatonic -> Bool
isChromatic = not . isDiatonic

isChromaticSharp :: Diatonic -> Bool
isChromaticSharp (Diatonic { dia_alt = a }) = a > 0

isChromaticFlat :: Diatonic -> Bool
isChromaticFlat (Diatonic { dia_alt = a }) = a < 0



nubAlteration :: Diatonic -> Diatonic
nubAlteration d = d { dia_alt = 0 }

toScaleDegree :: Int -> ScaleDegree
toScaleDegree n = fn $ n `modS1` 7
  where
    fn 1  = TONIC 
    fn 2  = SUPERTONIC 
    fn 3  = MEDIANT
    fn 4  = SUBDOMINANT 
    fn 5  = DOMINANT 
    fn 6  = SUBMEDIANT 
    fn 7  = LEADING_TONE
    fn _  = error "toScaleDegree - unreachable"



fromScaleDegree :: ScaleDegree -> Int
fromScaleDegree TONIC           = 1
fromScaleDegree SUPERTONIC      = 2
fromScaleDegree MEDIANT         = 3
fromScaleDegree SUBDOMINANT     = 4
fromScaleDegree DOMINANT        = 5
fromScaleDegree SUBMEDIANT      = 6
fromScaleDegree LEADING_TONE    = 7


scaleDegreeInt :: Diatonic -> Int
scaleDegreeInt = fromScaleDegree . dia_scale_degree

-- | Note - there is no reverse translation as index is not 
-- unique.
--
chromaticIndex :: (ScaleDegree -> Int) -> Diatonic -> Int
chromaticIndex fn (Diatonic { dia_scale_degree  = sd
                            , dia_alt           = alt
                            , dia_octave        = ove }) = 
    fn sd + fromIntegral alt + (12*ove)


-- | Ignores chromatic alteration.
--
diatonicIndex :: Diatonic -> Int
diatonicIndex (Diatonic { dia_scale_degree  = sd
                        , dia_octave        = ove }) = 
    fromScaleDegree sd +(7*ove)



alterationBetween :: Alteration -> Alteration -> Alt
alterationBetween a b = Alt $ fromAlteration a - fromAlteration b



-- | Scale is ordered pitches,  so we can count through them...
--
identifyScaleDegree :: Pitch -> Scale -> ScaleDegree
identifyScaleDegree pch sc = step TONIC $ fromScale sc
  where
    lettr           = pitch_letter $ pitch_name pch    
    step _  []      = error $ "unidentified scale degree"
    step sd (p:ps)  | pitch_letter p == lettr = sd
                    | otherwise               = step (succN 1 sd) ps


toDiatonic :: Scale -> Pitch -> Diatonic
toDiatonic sc pch = 
    Diatonic { dia_scale_degree = identifyScaleDegree pch sc
             , dia_alt          = scaleToneAlteration (pitch_name pch) sc
             , dia_octave       = pitch_octave pch }




fromDiatonic :: Scale -> Diatonic -> Pitch
fromDiatonic sc dia = 
    Pitch { pitch_name   = name2
          , pitch_octave = dia_octave dia
          }
  where
    name1 = lookupScaleDegree (dia_scale_degree dia) (buildDiatonicScale sc)
    name2 = alterBy name1 (dia_alt dia)


alterBy :: PitchName -> Alt -> PitchName
alterBy name alt = name { pitch_alteration = fn $ pitch_alteration name }
  where
    fn a = toAlteration (fromAlteration a + alt)


scaleToneAlteration :: PitchName -> Scale -> Alt
scaleToneAlteration (PitchName l a) sc = 
    case findAlteration l sc of
      Nothing -> error $ "scaleToneAlteration - ill-formed scale: " ++ show sc
      Just alt -> alterationBetween a alt


--------------------------------------------------------------------------------
-- Intervals


data SimpleInterval = UNISON | SECOND | THIRD   | FOURTH 
                    | FIFTH  | SIXTH  | SEVENTH
  deriving (Data,Eq,Ord,Show,Typeable)


data DiatonicInterval = DiatonicInterval
    { dia_interval_type         :: !SimpleInterval
    , dia_interval_octave       :: !Int
    }
  deriving (Data,Eq,Show,Typeable)


-- | Octave always positive...
--
fromDiatonicInterval :: DiatonicInterval -> Int
fromDiatonicInterval (DiatonicInterval { dia_interval_type = simple
                                       , dia_interval_octave = o }) = 
    o * 7 + fromSimpleInterval simple

fromSimpleInterval :: Integral a => SimpleInterval -> a
fromSimpleInterval UNISON      = 1
fromSimpleInterval SECOND      = 2
fromSimpleInterval THIRD       = 3
fromSimpleInterval FOURTH      = 4
fromSimpleInterval FIFTH       = 5
fromSimpleInterval SIXTH       = 6
fromSimpleInterval SEVENTH     = 7


toDiatonicInterval :: Int -> DiatonicInterval
toDiatonicInterval i = 
    DiatonicInterval { dia_interval_type   = toSimpleInterval i
                     , dia_interval_octave = i `divS1` 7 
                     }


toSimpleInterval :: (Show a, Integral a) => a -> SimpleInterval
toSimpleInterval i = fn $ i `modS1` 7
  where
    fn 1 = UNISON
    fn 2 = SECOND
    fn 3 = THIRD
    fn 4 = FOURTH
    fn 5 = FIFTH
    fn 6 = SIXTH
    fn 7 = SEVENTH
    fn n = error $ "toSimpleInterval - unreachable: " ++ show n


addDiatonicInterval :: Diatonic -> DiatonicInterval -> Diatonic
addDiatonicInterval dia ivl = 
     Diatonic { dia_scale_degree  = sd
              , dia_alt           = dia_alt dia
              , dia_octave        = ove 
              }
   where
     DiaCore sd ove = addDiatonicIntervalD1 (extractCore dia) ivl

subDiatonicInterval :: Diatonic -> DiatonicInterval -> Diatonic
subDiatonicInterval dia ivl = 
     Diatonic { dia_scale_degree  = sd
              , dia_alt           = dia_alt dia
              , dia_octave        = ove 
              }
   where
     DiaCore sd ove = subDiatonicIntervalD1 (extractCore dia) ivl



addDiatonicIntervalD1 :: DiaCore -> DiatonicInterval -> DiaCore
addDiatonicIntervalD1 dia1 ivl = 
    DiaCore { dc_scale_degree = sd, dc_octave = ove + dia_interval_octave ivl }
  where
    DiaCore sd ove = addSimpleInterval dia1 (dia_interval_type ivl)
    

subDiatonicIntervalD1 :: DiaCore -> DiatonicInterval -> DiaCore
subDiatonicIntervalD1 dia1 ivl = 
    DiaCore { dc_scale_degree = sd, dc_octave = ove - dia_interval_octave ivl }
  where
    DiaCore sd ove = subSimpleInterval dia1 (dia_interval_type ivl)
    


addSimpleInterval :: DiaCore -> SimpleInterval -> DiaCore
addSimpleInterval (DiaCore { dc_scale_degree = sd
                             , dc_octave       = ove }) ivl = 
    DiaCore { dc_scale_degree = sd1, dc_octave = ove1 }
  where
    sd1  = addSimpleInterval1 sd ivl
    ove1 = if sd1 < sd then ove+1 else ove

subSimpleInterval :: DiaCore -> SimpleInterval -> DiaCore
subSimpleInterval (DiaCore { dc_scale_degree = sd
                           , dc_octave       = ove }) ivl = 
    DiaCore { dc_scale_degree = sd1, dc_octave = ove1 }
  where
    sd1  = subSimpleInterval1 sd ivl
    ove1 = if sd1 > sd then ove-1 else ove

addSimpleInterval1 :: ScaleDegree -> SimpleInterval -> ScaleDegree
addSimpleInterval1 sd ivl = succN (fromSimpleInterval ivl - 1) sd

subSimpleInterval1 :: ScaleDegree -> SimpleInterval -> ScaleDegree
subSimpleInterval1 sd ivl = precN (fromSimpleInterval ivl - 1) sd


-- | Always positive.
--
diatonicIntervalBetween :: Diatonic -> Diatonic -> DiatonicInterval
diatonicIntervalBetween a b = fn (toIndex a) (toIndex b)
  where
    fn x y | y >= x     = toDiatonicInterval $ 1 + (y - x)
           | otherwise  = toDiatonicInterval $ 1 + (x - y)

    toIndex :: Diatonic -> Int
    toIndex (Diatonic d _ o) = fromScaleDegree d + (7*o)


transposeDiatonically :: Key -> DiatonicInterval -> Pitch -> Pitch 
transposeDiatonically key ivl pch = 
    let sc = buildScale key
        dp = toDiatonic sc pch
    in fromDiatonic sc $ dp `addDiatonicInterval` ivl




--------------------------------------------------------------------------------
-- Pretty


instance Pretty ScaleDegree where
  pPrint s = char '^' <> int (fromScaleDegree s)


instance Pretty SimpleInterval where
  pPrint UNISON         = text "unison"
  pPrint SECOND         = text "second"
  pPrint THIRD          = text "third"
  pPrint FOURTH         = text "fourth"
  pPrint FIFTH          = text "fifth"
  pPrint SIXTH          = text "sixth"
  pPrint SEVENTH        = text "seventh"

instance Pretty DiatonicInterval where
  pPrint (DiatonicInterval { dia_interval_type   = ty
                           , dia_interval_octave = o }) = prefix o <> pPrint ty
      where
        prefix i | i == 0    = empty
                 | i <  0    = parens (int i) <> char '*'
                 | otherwise = int i <> char '*'