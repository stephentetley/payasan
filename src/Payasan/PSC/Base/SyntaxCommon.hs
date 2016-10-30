{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Base.SyntaxCommon
-- Copyright   :  (c) Stephen Tetley 2015-2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Common syntax elements
--
--------------------------------------------------------------------------------

module Payasan.PSC.Base.SyntaxCommon
  ( 

    ScoreInfo(..)
  , StaffInfo(..)
  , Clef(..)

  , default_score_info
  , default_staff_info 

  , SectionInfo(..)
  , UnitNoteLength(..)

  , Tie(..)
  , Anno(..)
  , AnnoDU(..)

  , Time(..)

  , default_section_info

  , barLength

  , MeterPattern
  , TupletSpec(..)
  , tupletUnitRDuration 


  -- * Keys
  , c_maj

  ) where

import Payasan.Base.Basis
import Payasan.Base.Duration
import Payasan.Base.Pitch
import Payasan.Base.Scale


import Text.PrettyPrint.HughesPJ hiding (Mode)               -- package: pretty

import Data.Data
import Data.Ratio




-- | Note - don\'t store LilyPond Absolute / Relative pitch
-- at bar level. This a a global property as we must render
-- in one mode only.

-- Some data is usefully render info but should be specified by
-- library writer not by the end user, e.g. beaming
--
-- Potentially there is a relationship between a user exposed 
-- config and a larger internal one like Parsec\'s LanguageDef 
-- and TokenParser
--
-- CORRECTION - AbsPitch / Relative Pitch is actually a voice level
-- property.
--


data ScoreInfo = ScoreInfo
    { score_title               :: !String
    , score_ly_version          :: !String
    }
  deriving (Data,Eq,Show,Typeable)

-- | TODO - are there any more fields to add to this?
-- If not we should just use Clef.
--
data StaffInfo = StaffInfo 
    { staff_clef                :: !Clef
    }
  deriving (Data,Eq,Show,Typeable)


data Clef = TREBLE | BASS
  deriving (Data,Eq,Show,Typeable)


default_score_info :: ScoreInfo
default_score_info = ScoreInfo
    { score_title               = ""
    , score_ly_version          = "2.18.2"
    }


default_staff_info :: StaffInfo
default_staff_info = StaffInfo 
    { staff_clef                = TREBLE
    }


-- | Note - @SectionInfo@ is stored as a header to a Bar in
-- Beam and Main syntax.
-- 
-- This allows concatenating bars together (and tempo and key 
-- changes). Generally outputting must be sensitive to changes 
-- to SectionInfo as a new bar is printed.
-- 
-- Elementary syntax is intended for transformation, hence we 
-- put @SectionInfo@ at the start of the phrase
--

data SectionInfo = SectionInfo
    { section_key               :: !Key
    , section_meter             :: !Meter
    , section_meter_pattern     :: !MeterPattern
    , section_unit_note_len     :: !UnitNoteLength
    , section_bpm               :: !BPM
    }
  deriving (Data,Eq,Show,Typeable)


data Tie = TIE | NO_TIE
  deriving (Data,Enum,Eq,Ord,Show,Typeable)



class Anno a where anno :: a -> Doc

instance Anno () where anno = const empty


data AnnoDU a = AnnoDU { defs :: Doc, use :: a -> Doc }




-- TODO - span bars with meter pattern (e.g. for 2-bar patterns
-- as in Latin music).
--
type MeterPattern = [RDuration]

data UnitNoteLength = UNIT_NOTE_4 | UNIT_NOTE_8 | UNIT_NOTE_16
  deriving (Data,Enum,Eq,Ord,Show,Typeable)







default_section_info :: SectionInfo
default_section_info = SectionInfo 
    { section_key               = c_maj
    , section_meter             = TimeSig $ Time 4 4 
    , section_meter_pattern     = [1%2,1%2]
    , section_unit_note_len     = UNIT_NOTE_8
    , section_bpm               = 120
    }






barLength :: Time -> RDuration
barLength (Time n d) = (fromIntegral n) * fn d
  where
    fn i = 1 % fromIntegral i


-- | @tuplet_time_mult@ is the multipler of the note 
-- length (ideally all notes _should_ be the same length).
--
data TupletSpec = TupletSpec
     { tuplet_num       :: !Int
     , tuplet_time_mult :: !Int
     , tuplet_len       :: !Int
     }
  deriving (Data,Eq,Show,Typeable)


tupletUnitRDuration :: TupletSpec -> RDuration -> RDuration 
tupletUnitRDuration (TupletSpec { tuplet_time_mult = m }) unitd = 
    unitd * realToFrac m




  

c_maj :: Key
c_maj = Key (PitchName C NAT) MAJOR
