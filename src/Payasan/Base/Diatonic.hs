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

  , isDiatonic
  , isChromatic

  , toDiatonic
  , fromDiatonic
  , noAlteration

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




data Diatonic = Diatonic 
    { dia_scale_degree  :: !ScaleDegree
    , dia_alt           :: !Alt
    , dia_octave        :: !Int
    }
  deriving (Data,Eq,Show,Typeable)


newtype Alt = Alt Int
  deriving (Data,Enum,Eq,Integral,Num,Ord,Real,Show,Typeable)


isDiatonic :: Diatonic -> Bool
isDiatonic (Diatonic { dia_alt = a }) = a == 0

isChromatic :: Diatonic -> Bool
isChromatic = not . isDiatonic


noAlteration :: Diatonic -> Diatonic
noAlteration d = d { dia_alt = 0 }

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


keyRoot :: Key -> Pitch
keyRoot (Key start _) = nearestRootToC4 start 

tonic_root :: Diatonic
tonic_root = Diatonic { dia_scale_degree = TONIC
                      , dia_alt          = 0
                      , dia_octave       = 0 }



toDiatonic :: Key -> Pitch -> Diatonic
toDiatonic key p = dia1 { dia_alt = alt }
  where
    root        = keyRoot key
    op          = if p `isHigher` root then addDiatonicInterval 
                                       else subDiatonicInterval
    arith_dist  = interval_distance $ intervalBetween root p
    alt         = scaleToneAlteration (pitch_name p) key
    dia1        = tonic_root `op` toDiatonicInterval arith_dist

fromDiatonic :: Key -> Diatonic -> Pitch
fromDiatonic key (Diatonic deg alt o) = 
    alterBy (Pitch name (root_ove + o + carry)) alt
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
diatonicIntervalBetween :: Diatonic -> Diatonic -> DiatonicInterval
diatonicIntervalBetween a b = fn (toIndex a) (toIndex b)
  where
    fn x y | y >= x     = toDiatonicInterval $ 1 + (y - x)
           | otherwise  = toDiatonicInterval $ 1 + (x - y)

    toIndex :: Diatonic -> Int
    toIndex (Diatonic d _ o) = fromScaleDegree d + (7*o)

addDiatonicInterval :: Diatonic -> DiatonicInterval -> Diatonic
addDiatonicInterval (Diatonic deg alt ove) ivl = 
    Diatonic deg1 alt ove1
  where
    deg1  = addDiatonicInterval' deg ivl
    carry = if deg1 < deg then 1 else 0
    ove1  = ove + carry + dia_interval_octave ivl



addDiatonicInterval' :: ScaleDegree -> DiatonicInterval -> ScaleDegree
addDiatonicInterval' sd ivl = toScaleDegree $ i + j
  where
    i = fromScaleDegree sd 
    j = fromDiatonicInterval ivl



subDiatonicInterval :: Diatonic -> DiatonicInterval -> Diatonic
subDiatonicInterval (Diatonic deg alt ove) ivl = 
    Diatonic deg1 alt ove1
  where
    deg1  = subDiatonicInterval' deg ivl
    carry = if deg1 > deg then (-1) else 0
    ove1  = ove + carry - dia_interval_octave ivl



subDiatonicInterval' :: ScaleDegree -> DiatonicInterval -> ScaleDegree
subDiatonicInterval' sd ivl = toScaleDegree $ i - j
  where
    i = fromScaleDegree sd 
    j = fromDiatonicInterval ivl





transposeDiatonically :: Key -> DiatonicInterval -> Pitch -> Pitch 
transposeDiatonically key ivl p = 
    let dp = toDiatonic key p 
    in fromDiatonic key $ dp `addDiatonicInterval` ivl




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