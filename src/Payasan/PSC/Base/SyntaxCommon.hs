{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Base.SyntaxCommon
-- Copyright   :  (c) Stephen Tetley 2015-2017
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

    TyDoc(..)
  , TyText(..)
  
  , ScoreInfo(..)
  , Clef(..)

  , default_score_info

  , SectionInfo(..)
  , UnitNoteLength(..)

  , Tie(..)
  , Anno(..)
  , AnnoDU(..)

  , default_section_info

  , barRatDuration

  , MeterPattern
  , TupletSpec(..)
  , tupletUnitRatDuration 


  -- * Keys
  , c_maj

  ) where

import Payasan.Base.Basis
import Payasan.Base.Duration
import Payasan.Base.Pitch
import Payasan.Base.Scale


import Text.PrettyPrint.HughesPJ hiding (Mode)               -- package: pretty


import qualified Data.Text as TEXT

import Data.Data
import Data.Ratio

data TyDoc a = TyDoc { extractDoc :: Doc }


data TyText a = TyText { extractText :: TEXT.Text }






--------------------------------------------------------------------------------
-- Score syntax elements


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



-- | DEPRECATED - move to ABC and LilyPond _compilers_.
data ScoreInfo = ScoreInfo
    { score_title               :: !String
    , score_ly_version          :: !String
    }
  deriving (Data,Eq,Show,Typeable)




  
-- | Consider this DEPRECATED.
-- Clef should be thought of as a /print annotation/ and not
-- be introspected upon, thus it would be fine as a String
-- which is extensible.
--
data Clef = TREBLE | BASS
  deriving (Data,Eq,Show,Typeable)



default_score_info :: ScoreInfo
default_score_info = ScoreInfo
    { score_title               = ""
    , score_ly_version          = "2.18.2"
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
-- TODO - SectionInfo is probably the wrong mechanism to 
-- annotate sections as some of the infos are already backend 
-- specific and others are representation specific (e.g.
-- percussion doesn't have a key).
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
type MeterPattern = [RatDuration]

data UnitNoteLength = UNIT_NOTE_4 | UNIT_NOTE_8 | UNIT_NOTE_16
  deriving (Data,Enum,Eq,Ord,Show,Typeable)




default_section_info :: SectionInfo
default_section_info = SectionInfo 
    { section_key               = c_maj
    , section_meter             = Metered $ TimeSig 4 4 
    , section_meter_pattern     = [1%2,1%2]
    , section_unit_note_len     = UNIT_NOTE_8
    , section_bpm               = 120
    }






barRatDuration :: TimeSig -> RatDuration
barRatDuration (TimeSig n d) = (fromIntegral n) * fn d
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


tupletUnitRatDuration :: TupletSpec -> RatDuration -> RatDuration 
tupletUnitRatDuration (TupletSpec { tuplet_time_mult = m }) unitd = 
    unitd * realToFrac m




  

c_maj :: Key
c_maj = Key (PitchName C NAT) MAJOR
