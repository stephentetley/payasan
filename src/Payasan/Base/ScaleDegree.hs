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
  (
    ScaleDegree(..)
  , ScaleStep(..)
  , OveScaleStep(..)   
  , DiatonicInterval(..)
  , SimpleInterval(..)

  , SpellingMap
  , buildSpellingMap

  , nextScaleDegree
  , prevScaleDegree
  , nthScaleDegreeFwd
  , nthScaleDegreeBwd

  , fromDiatonicInterval
  , toDiatonicInterval

  , diatonicIntervalBetween

  , fromSimpleInterval  -- TEMP
  , toSimpleInterval    -- TEMP

  , toPitch1
  , toPitch
  , fromPitch1
  , fromPitch

  , addDiatonicInterval
  , subDiatonicInterval

  ) where


import Payasan.Base.Internal.Base
import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.Scale
import Payasan.Base.Pitch


import Data.Data
import qualified Data.Map as MAP


--------------------------------------------------------------------------------
-- Syntax

data ScaleDegree = TONIC | SUPERTONIC | MEDIANT | SUBDOMINANT 
                 | DOMINANT | SUBMEDIANT | LEADING_TONE
  deriving (Bounded,Data,Enum,Eq,Ord,Show,Typeable)


data ScaleStep = ScaleStep
     { step_degree      :: !ScaleDegree
     , step_alteration  :: !Alt 
     }
  deriving (Data,Eq,Show,Typeable)


newtype Alt = Alt Int
  deriving (Data,Enum,Eq,Integral,Num,Ord,Real,Show,Typeable)

-- | Notion of ord is complicated here, it depends on what octave 
-- means...
--
data OveScaleStep = OveScaleStep ScaleStep Octave
  deriving (Data,Eq,Show,Typeable)


data DiatonicInterval = DiatonicInterval
    { dia_interval_type         :: !SimpleInterval
    , dia_interval_octave       :: !Int
    }
  deriving (Data,Eq,Show,Typeable)

data SimpleInterval = UNISON | SECOND | THIRD | FOURTH 
                    | FIFTH | SIXTH | SEVENTH
  deriving (Data,Eq,Ord,Show,Typeable)



data SpellingMap = SpellingMap 
    { spelling_deg_to_pch       :: MAP.Map ScaleDegree PitchName
    , spelling_pch_to_step      :: MAP.Map PitchLetter ScaleStep
    }
  deriving (Show)


ordered_degrees :: [ScaleDegree]
ordered_degrees = [ TONIC .. LEADING_TONE ]


-- Design notes
-- Using a spelling map seems easier - more tangible (if slower)
-- Lookup failure is signalled with an out-of-band value


buildSpellingMap :: Key -> SpellingMap 
buildSpellingMap key = let scale = buildScale key in 
    SpellingMap { spelling_deg_to_pch   = makePitchLookup scale
                , spelling_pch_to_step  = makeStepLookup scale
                }



makePitchLookup :: Scale -> MAP.Map ScaleDegree PitchName
makePitchLookup ss = MAP.fromList $ zip ordered_degrees ss

makeStepLookup :: Scale -> MAP.Map PitchLetter ScaleStep
makeStepLookup ss = 
    MAP.fromList $ zipWith fn ss ordered_degrees
  where
    fn (PitchName lttr alt) deg = (lttr, ScaleStep deg $ fromAlteration alt)





nextScaleDegree :: ScaleDegree -> ScaleDegree
nextScaleDegree LEADING_TONE    = TONIC
nextScaleDegree sd              = succ sd

prevScaleDegree :: ScaleDegree -> ScaleDegree
prevScaleDegree TONIC           = LEADING_TONE
prevScaleDegree sd              = pred sd


nthScaleDegreeFwd :: Int -> ScaleDegree -> ScaleDegree
nthScaleDegreeFwd n sd 
    | n == 0    = sd
    | n <  0    = nthScaleDegreeBwd (abs n) sd
    | otherwise = let n1 = n `mod` maxi 
                      n2 = (fromEnum sd + n1) `mod` maxi
                  in toEnum n2
  where
    maxi = 1 + fromEnum (maxBound :: ScaleDegree) 

nthScaleDegreeBwd :: Int -> ScaleDegree -> ScaleDegree
nthScaleDegreeBwd n sd 
    | n == 0    = sd
    | n <  0    = nthScaleDegreeFwd (abs n) sd
    | otherwise = let n1 = n `mod` maxi 
                      n2 = (fromEnum sd - n1) `mod` maxi
                  in toEnum n2
  where
    maxi = 1 + fromEnum (maxBound :: ScaleDegree) 




-- Semitone counting is not supported as it is context-dependent 
-- on key.



-- Could we achieve same as above with a @diatonic_root_note@
-- rather than a key?
--
-- Then OveScaleStep always has a ctx-free /measurement/ 
-- interpretation. (Root will be needed for change to Pitch).
--

-- NOTE - arbitrary key change (within phrases) makes using 
-- scale degrees as a representation a lot more complicated.
-- There is good justification or moving LocalRenderInfo
-- to Phrase rather than Bar for Monnophonic note list.
-- 


-- | A diatonic interval has a unique numeric representation
-- 
diatonicIntervalBetween :: OveScaleStep -> OveScaleStep -> DiatonicInterval
diatonicIntervalBetween a b 
    | a `equivalent` b  = DiatonicInterval UNISON 0
    | a `isLower` b     = toDiatonicInterval $ 1 + naturalPosition b - naturalPosition a
    | otherwise         = toDiatonicInterval $ 1 + naturalPosition a - naturalPosition b


instance PitchOrd OveScaleStep where 
  equivalent a b = naturalPosition a == naturalPosition b
  isLower    a b = naturalPosition a <  naturalPosition b
  isHigher   a b = naturalPosition a >  naturalPosition b

naturalPosition :: OveScaleStep -> Int
naturalPosition (OveScaleStep s o) = o * 7 + fromEnum (step_degree s)


-- | Octave always positive...
--
fromDiatonicInterval :: DiatonicInterval -> Int
fromDiatonicInterval (DiatonicInterval { dia_interval_type = simple
                                       , dia_interval_octave = o }) = 
    o * 7 + fromSimpleInterval simple

toDiatonicInterval :: Int -> DiatonicInterval
toDiatonicInterval i = 
    DiatonicInterval { dia_interval_type   = toSimpleInterval i
                     , dia_interval_octave = i `div` 7 
                     }


fromSimpleInterval :: Integral a => SimpleInterval -> a
fromSimpleInterval UNISON      = 1
fromSimpleInterval SECOND      = 2
fromSimpleInterval THIRD       = 3
fromSimpleInterval FOURTH      = 4
fromSimpleInterval FIFTH       = 5
fromSimpleInterval SIXTH       = 6
fromSimpleInterval SEVENTH     = 7

toSimpleInterval :: (Show a, Integral a) => a -> SimpleInterval
toSimpleInterval i = fn $ 1 + ((i-1) `mod` 7)
  where
    fn 1 = UNISON
    fn 2 = SECOND
    fn 3 = THIRD
    fn 4 = FOURTH
    fn 5 = FIFTH
    fn 6 = SIXTH
    fn 7 = SEVENTH
    fn n = error $ "toSimpleInterval - unreachable: " ++ show n





toPitch1 :: SpellingMap -> OveScaleStep -> Pitch
toPitch1 sm  (OveScaleStep deg o) = 
    case MAP.lookup (step_degree deg) (spelling_deg_to_pch sm) of
        Just name -> Pitch (alterPitchName name $ step_alteration deg) o
        Nothing -> Pitch c_nat 0
 
alterPitchName :: PitchName -> Alt -> PitchName 
alterPitchName (PitchName l a) n = 
    PitchName l $ toAlteration (n + fromAlteration a)



fromPitch1 :: SpellingMap -> Pitch -> OveScaleStep
fromPitch1 sm (Pitch nm o) = 
    case MAP.lookup (pitch_letter nm) (spelling_pch_to_step sm) of
        Nothing -> OveScaleStep (ScaleStep TONIC 0) 0
        Just sd -> OveScaleStep (alterScaleStep sd $ pitch_alteration nm) o

alterScaleStep :: ScaleStep -> Alteration -> ScaleStep
alterScaleStep (ScaleStep name a) alt = 
    ScaleStep name (a + fromAlteration alt)


-- Prefer fromPitch1 and toPitch1...

fromPitch :: Key -> Pitch -> OveScaleStep
fromPitch key p = fromPitch1 (buildSpellingMap key) p

toPitch :: Key -> OveScaleStep -> Pitch
toPitch key sd = toPitch1 (buildSpellingMap key) sd






addDiatonicInterval :: OveScaleStep -> DiatonicInterval -> OveScaleStep
addDiatonicInterval (OveScaleStep step o) ivl = 
   let (carry,step1) = addSimpleInterval step (dia_interval_type ivl)
       om            = dia_interval_octave ivl
   in OveScaleStep step1 (o + om + carry)


-- | carry is 1 or 0
--
addSimpleInterval :: ScaleStep -> SimpleInterval -> (Int,ScaleStep)
addSimpleInterval (ScaleStep name _) ivl = 
    let name1 = nthScaleDegreeFwd (fromSimpleInterval ivl - 1) name
    in if name1 < name then (1, ScaleStep name1 0) 
                       else (0, ScaleStep name1 0)

subDiatonicInterval :: OveScaleStep -> DiatonicInterval -> OveScaleStep
subDiatonicInterval (OveScaleStep step o) ivl = 
   let (carry,step1) = subSimpleInterval step (dia_interval_type ivl)
       om            = dia_interval_octave ivl
   in OveScaleStep step1 ((o - om) + carry)


-- | carry is (-1) or 0
--
subSimpleInterval :: ScaleStep -> SimpleInterval -> (Int,ScaleStep)
subSimpleInterval (ScaleStep name _) ivl = 
    let name1 = nthScaleDegreeBwd (fromSimpleInterval ivl - 1) name
    in if name1 > name then (-1, ScaleStep name1 0) 
                       else (0, ScaleStep name1 0)


