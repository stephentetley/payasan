{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.ScaleDegree
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

module Payasan.Base.ScaleDegree
  (
    DiatonicScale
  , fromDiatonicScale
  , buildDiatonicScale

  , ScaleDegree(..)
  , DiatonicPitch(..)
  , ChromaticPitch(..)

  , toIndex
  , fromIndex

  , toDiatonicPitch
  , fromDiatonicPitch

  , SimpleInterval(..)
  , DiatonicInterval(..)

  , toChromaticPitch
  , fromChromaticPitch
  , toDiatonicInterval
  , fromDiatonicInterval

  , diatonicIntervalBetween
  , addDiatonicInterval
  , subDiatonicInterval

  , diatonically
  , transposeWithDiatonicInterval

  ) where


import Payasan.Base.Internal.Base
import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.Scale
import Payasan.Base.Internal.Utils

import Payasan.Base.Pitch

import Text.PrettyPrint.HughesPJClass           -- package: pretty

import Data.Data
import qualified Data.Map as MAP


--------------------------------------------------------------------------------
-- Syntax

newtype DiatonicScale = DiatonicScale { 
    fromDiatonicScale :: MAP.Map ScaleDegree PitchName }
  deriving (Data,Eq,Show,Typeable)


buildDiatonicScale :: Key -> DiatonicScale
buildDiatonicScale key = DiatonicScale $ 
    MAP.fromList $ zip [TONIC .. LEADING_TONE] (fromScale $ buildScale key)


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

-- TODO - naming is bad, the differentiation between Diatonic 
-- pitch and Chromatic pitch is artificial and not present in 
-- the literature.


 
data DiatonicPitch = DiatonicPitch
    { dp_scale_degree   :: !ScaleDegree
    , dp_octave         :: !Int
    }
  deriving (Data,Eq,Show,Typeable)

data ChromaticPitch = ChromaticPitch
    { diatonic_base     :: !DiatonicPitch
    , chromatic_alt     :: !Alt
    }
  deriving (Data,Eq,Show,Typeable)


newtype Alt = Alt Int
  deriving (Data,Enum,Eq,Integral,Num,Ord,Real,Show,Typeable)


-- WRONG - see below
instance PitchOrd ChromaticPitch where 
  equivalent a b = chromaticIndex a == chromaticIndex b
  isLower    a b = chromaticIndex a <  chromaticIndex b
  isHigher   a b = chromaticIndex a >  chromaticIndex b


instance PitchOrd DiatonicPitch where 
  equivalent a b = toIndex a == toIndex b
  isLower    a b = toIndex a <  toIndex b
  isHigher   a b = toIndex a >  toIndex b


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


-- This is wrong - chromatic alteration is too large (cf Z12) 
-- to be added to diatonic (cf Z7).
--
-- To get diatonic in Z12, need to interpret w.r.t a Key (major, 
-- minor, other mode).
--
chromaticIndex :: ChromaticPitch -> Int
chromaticIndex (ChromaticPitch { diatonic_base = p
                               , chromatic_alt = a }) = 
    toIndex p + fromIntegral a


toIndex :: DiatonicPitch -> Int
toIndex (DiatonicPitch d o) = fromScaleDegree d + (7*o)


fromIndex :: Int -> DiatonicPitch
fromIndex i = 
    let (o,x) = i `divModS1` 7 
    in DiatonicPitch { dp_scale_degree = toScaleDegree x
                     , dp_octave       = o }




alterationBetween :: Alteration -> Alteration -> Alt
alterationBetween a b = Alt $ fromAlteration a - fromAlteration b


keyRoot :: Key -> Pitch
keyRoot (Key start _) = nearestRootToC4 start 

tonic_root :: DiatonicPitch 
tonic_root = DiatonicPitch { dp_scale_degree = TONIC
                           , dp_octave       = 0 }


toChromaticPitch :: Key -> Pitch -> ChromaticPitch
toChromaticPitch key p = ChromaticPitch { diatonic_base = dia_pitch
                                        , chromatic_alt = alt  }
  where
    dia_pitch   = toDiatonicPitch key p
    alt         = scaleToneAlteration (pitch_name p) key


-- | Loses resolution if input pitch is not a scale tone.
--
toDiatonicPitch :: Key -> Pitch -> DiatonicPitch
toDiatonicPitch key p = tonic_root `op` toDiatonicInterval arith_dist
  where
    root        = keyRoot key
    op          = if p `isHigher` root then addDiatonicInterval 
                                       else subDiatonicInterval
    arith_dist  = interval_distance $ intervalBetween root p
    

fromChromaticPitch :: Key -> ChromaticPitch -> Pitch
fromChromaticPitch key (ChromaticPitch root alt) = 
    let p1 = fromDiatonicPitch key root in alterBy p1 alt

fromDiatonicPitch :: Key -> DiatonicPitch -> Pitch
fromDiatonicPitch key (DiatonicPitch deg o) = Pitch name (root_ove + o + carry)
  where
    name        = lookupScaleDegree deg $ buildDiatonicScale key
    root        = keyRoot key
    root_ove    = pitch_octave root
    root_letter = pitch_letter $ pitch_name root
    carry       = octaveCarry root_letter (pitch_letter name)


-- | return 0 same octave or 1 (next octave)

octaveCarry :: PitchLetter -> PitchLetter -> Int
octaveCarry a b | a <= b    = 0
                | otherwise = 1


alterBy :: Pitch -> Alt -> Pitch
alterBy p alt = fn $ pitch_alteration $ pitch_name p
  where
    fn a = let i = fromAlteration a; a1 = toAlteration (i + alt)
           in setAlteration p a1


scaleToneAlteration :: PitchName -> Key -> Alt
scaleToneAlteration (PitchName l a) key = 
    let sc = buildScale key 
    in case findAlteration l sc of
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


-- | Always positive.
--
diatonicIntervalBetween :: DiatonicPitch -> DiatonicPitch -> DiatonicInterval
diatonicIntervalBetween a b = fn (toIndex a) (toIndex b)
  where
    fn x y | y >= x     = toDiatonicInterval $ 1 + (y - x)
           | otherwise  = toDiatonicInterval $ 1 + (x - y)


addDiatonicInterval :: DiatonicPitch -> DiatonicInterval -> DiatonicPitch
addDiatonicInterval p ivl = 
    fromIndex $ toIndex p + (fromDiatonicInterval ivl - 1)

subDiatonicInterval :: DiatonicPitch -> DiatonicInterval -> DiatonicPitch
subDiatonicInterval p ivl = 
    fromIndex $ toIndex p - (fromDiatonicInterval ivl - 1)



--- TODO - not sure about the API for these two...

diatonically :: (DiatonicPitch -> DiatonicPitch) 
             -> ChromaticPitch 
             -> ChromaticPitch
diatonically fn cp@(ChromaticPitch { diatonic_base = dp }) = 
    cp { diatonic_base = fn dp }

transposeWithDiatonicInterval :: Key -> DiatonicInterval -> Pitch -> Pitch 
transposeWithDiatonicInterval key ivl p = 
    let cp = toChromaticPitch key p 
    in fromChromaticPitch key $ diatonically (`addDiatonicInterval` ivl) cp


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