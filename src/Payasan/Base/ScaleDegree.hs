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
import Payasan.Base.Internal.Scale
import Payasan.Base.Pitch


import Data.Data
import qualified Data.Map as MAP


--------------------------------------------------------------------------------
-- Syntax

data Degree = TONIC | SUPERTONIC | MEDIANT | SUBDOMINANT 
            | DOMINANT | SUBMEDIANT | LEADING_TONE
  deriving (Bounded,Data,Enum,Eq,Ord,Show,Typeable)


data ScaleDegree = ScaleDegree 
     { sd_degree        :: !Degree
     , sd_alteration    :: !Alt 
     }
  deriving (Data,Eq,Show,Typeable)


newtype Alt = Alt Int
  deriving (Data,Enum,Eq,Integral,Num,Ord,Real,Show,Typeable)

-- | Notion of ord is complicated here, it depends on what octave 
-- means...
--
data OveScaleDegree = OveScaleDegree ScaleDegree Octave
  deriving (Data,Eq,Show,Typeable)



data IntervalType = UNISON | SECOND | THIRD | FOURTH | FIFTH | SIXTH | SEVENTH | OCTAVE
  deriving (Data,Eq,Ord,Show,Typeable)



data SpellingMap = SpellingMap 
    { spelling_deg_to_pch       :: MAP.Map Degree PitchName
    , spelling_pch_to_deg       :: MAP.Map PitchLetter ScaleDegree
    }
  deriving (Show)


ordered_degrees :: [Degree]
ordered_degrees = [ TONIC .. LEADING_TONE ]


-- Design notes
-- Using a spelling map seems easier - more tangible (if slower)
-- Lookup failure is signalled with an out-of-band value


buildSpellingMap :: Key -> SpellingMap 
buildSpellingMap key = let scale = buildScale key in 
    SpellingMap { spelling_deg_to_pch = makePitchLookup scale
                , spelling_pch_to_deg = makeDegreeLookup scale
                }



makePitchLookup :: Scale -> MAP.Map Degree PitchName
makePitchLookup ss = MAP.fromList $ zip ordered_degrees ss

makeDegreeLookup :: Scale -> MAP.Map PitchLetter ScaleDegree
makeDegreeLookup ss = 
    MAP.fromList $ zipWith fn ss ordered_degrees
  where
    fn (PitchName lttr alt) deg = (lttr, ScaleDegree deg $ fromAlteration alt)




toPitch1 :: SpellingMap -> OveScaleDegree -> Pitch
toPitch1 sm  (OveScaleDegree deg o) = 
    case MAP.lookup (sd_degree deg) (spelling_deg_to_pch sm) of
        Just name -> Pitch (alterPitchName name $ sd_alteration deg) o
        Nothing -> Pitch c_nat 0
 
alterPitchName :: PitchName -> Alt -> PitchName 
alterPitchName (PitchName l a) n = 
    PitchName l $ toAlteration (n + fromAlteration a)



fromPitch1 :: SpellingMap -> Pitch -> OveScaleDegree
fromPitch1 sm (Pitch nm o) = 
    case MAP.lookup (pitch_letter nm) (spelling_pch_to_deg sm) of
        Nothing -> OveScaleDegree (ScaleDegree TONIC 0) 0
        Just sd -> OveScaleDegree (alterScaleDegree sd $ pitch_alteration nm) o

alterScaleDegree :: ScaleDegree -> Alteration -> ScaleDegree
alterScaleDegree (ScaleDegree name a) alt = 
    ScaleDegree name (a + fromAlteration alt)


-- Prefer fromPitch1 and toPitch1...

fromPitch :: Key -> Pitch -> OveScaleDegree
fromPitch key p = fromPitch1 (buildSpellingMap key) p

toPitch :: Key -> OveScaleDegree -> Pitch
toPitch key sd = toPitch1 (buildSpellingMap key) sd