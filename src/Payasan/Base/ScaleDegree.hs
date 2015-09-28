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
  , nthScaleDegreeUp
  , nthScaleDegreeDown

  , fromDiatonicInterval
  , toDiatonicInterval

  , diatonicIntervalBetween


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
    { spelling_nat_to_scale_tone    :: MAP.Map PitchLetter PitchName }
  deriving (Show)



-- Design notes
-- Using a spelling map seems easier - more tangible (if slower)
-- Lookup failure is signalled with an out-of-band value


buildSpellingMap :: Key -> SpellingMap 
buildSpellingMap key = let scale = buildScale key in 
    SpellingMap { spelling_nat_to_scale_tone = makeSTLookup scale }



makeSTLookup :: Scale -> MAP.Map PitchLetter PitchName
makeSTLookup ss = MAP.fromList $ map fn ss
  where
    fn pn = (pitch_letter pn,pn)




nextScaleDegree :: ScaleDegree -> ScaleDegree
nextScaleDegree LEADING_TONE    = TONIC
nextScaleDegree sd              = succ sd

prevScaleDegree :: ScaleDegree -> ScaleDegree
prevScaleDegree TONIC           = LEADING_TONE
prevScaleDegree sd              = pred sd


nthScaleDegreeUp :: Int -> ScaleDegree -> ScaleDegree
nthScaleDegreeUp n sd 
    | n == 0    = sd
    | n <  0    = nthScaleDegreeDown (abs n) sd
    | otherwise = let n1 = n `mod` 7
                      n2 = (fromEnum sd + n1) `mod` 8
                  in toEnum n2

nthScaleDegreeDown :: Int -> ScaleDegree -> ScaleDegree
nthScaleDegreeDown n sd 
    | n == 0    = sd
    | n <  0    = nthScaleDegreeUp (abs n) sd
    | otherwise = let n1 = n `mod` 8
                      n2 = (fromEnum sd - n1) `mod` 8
                  in toEnum n2





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



-- Prefer fromPitch2 and toPitch2...

data Direction = UP | DOWN
  deriving (Eq,Show)


fromPitch :: Key -> Pitch -> OveScaleStep
fromPitch (Key start _) p = fromPitch1 root p
  where
    root = nearestRootToC4 start 

toPitch :: Key -> OveScaleStep -> Pitch
toPitch key@(Key start _) sd = toPitch1 (buildSpellingMap key) root sd
  where
    root = nearestRootToC4 start 

toPitch1 :: SpellingMap -> Pitch -> OveScaleStep -> Pitch
toPitch1 sm root oss = 
    alterPitch scale_tone alt
  where
    (dir,dia,alt) = asInterval oss
    adist         = fromDiatonicInterval dia
    Pitch lbl ove = if dir == UP then arithmeticNaturalUp adist root
                                 else arithmeticNaturalDown adist root

    scale_tone    = case MAP.lookup (pitch_letter lbl) (spelling_nat_to_scale_tone sm) of
                      Just name -> Pitch name ove
                      Nothing -> Pitch lbl ove



fromPitch1 :: Pitch -> Pitch -> OveScaleStep
fromPitch1 root pch = alterOveScaleStep ss1 (toAlteration alt)
  where
    dir         = if pch `isHigher` root then UP else DOWN
    real_ival   = if dir == UP then intervalBetween root pch 
                               else intervalBetween pch root

    nat_ival    = if dir == UP then intervalBetween root (naturalOf pch)
                               else intervalBetween (naturalOf pch) root

    alt         = interval_semitones real_ival - interval_semitones nat_ival
    
    ss1         = asScaleStep $ toDiatonicInterval $ interval_distance nat_ival

    


-- Essentially OveScaleStep and DiatonicInterval are the same
-- (OctaveScaleStep accommodates non scale notes via alterations)
-- To make a Pitch (from the scale root) turn the scale step into 
-- an interval and add it to the pitch.


asInterval :: OveScaleStep -> (Direction,DiatonicInterval,Alt)
asInterval (OveScaleStep (ScaleStep name alt) ove) = 
    (direction, DiatonicInterval (fromName name) (abs ove), alt)
  where
    direction                   = if ove < 0 then DOWN else UP
    fromName TONIC              = UNISON
    fromName SUPERTONIC         = SECOND 
    fromName MEDIANT            = THIRD
    fromName SUBDOMINANT        = FOURTH
    fromName DOMINANT           = FIFTH
    fromName SUBMEDIANT         = SIXTH
    fromName LEADING_TONE       = SEVENTH


asScaleStep :: DiatonicInterval -> OveScaleStep
asScaleStep (DiatonicInterval name o) = 
    OveScaleStep (ScaleStep (fromIval name ) 0) o
  where
    fromIval UNISON             = TONIC
    fromIval SECOND             = SUPERTONIC
    fromIval THIRD              = MEDIANT
    fromIval FOURTH             = SUBDOMINANT
    fromIval FIFTH              = DOMINANT
    fromIval SIXTH              = SUBMEDIANT
    fromIval SEVENTH            = LEADING_TONE
  


 

alterPitch :: Pitch -> Alt -> Pitch
alterPitch (Pitch name o) alt = Pitch (alterPitchName name alt) o

alterPitchName :: PitchName -> Alt -> PitchName 
alterPitchName (PitchName l a) n = 
    PitchName l $ toAlteration (n + fromAlteration a)



alterOveScaleStep :: OveScaleStep -> Alteration -> OveScaleStep
alterOveScaleStep (OveScaleStep ss o) alt = 
    OveScaleStep (alterScaleStep ss alt) o

alterScaleStep :: ScaleStep -> Alteration -> ScaleStep
alterScaleStep (ScaleStep name a) alt = 
    ScaleStep name (a + fromAlteration alt)




addDiatonicInterval :: OveScaleStep -> DiatonicInterval -> OveScaleStep
addDiatonicInterval (OveScaleStep step o) ivl = 
   let (carry,step1) = addSimpleInterval step (dia_interval_type ivl)
       om            = dia_interval_octave ivl
   in OveScaleStep step1 (o + om + carry)


-- | carry is 1 or 0
--
addSimpleInterval :: ScaleStep -> SimpleInterval -> (Int,ScaleStep)
addSimpleInterval (ScaleStep name _) ivl = 
    let name1 = nthScaleDegreeUp (fromSimpleInterval ivl - 1) name
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
    let name1 = nthScaleDegreeDown (fromSimpleInterval ivl - 1) name
    in if name1 > name then (-1, ScaleStep name1 0) 
                       else (0, ScaleStep name1 0)


