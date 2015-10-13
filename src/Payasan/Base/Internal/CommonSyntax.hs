{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.CommonSyntax
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Common syntax elements
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.CommonSyntax
  ( 

    ScoreInfo(..)
  , OctaveMode(..)
  , Clef(..)

  , default_score_info

  , LocalContextInfo(..)
  , UnitNoteLength(..)
  , Key(..)
  , Mode(..)
  , Meter(..)

  , default_local_info

  , barLength

  , MeterPattern
  , TupletSpec(..)
  , tupletUnitRDuration 


  -- * Keys
  , c_maj

  ) where

import Payasan.Base.Internal.Base

import Payasan.Base.Duration
import Payasan.Base.Pitch


import Data.Data
import Data.Ratio


-- | Note - don\'t store LilyPond Absolute / Relative pitch
-- at bar level. This a a global property as we must render
-- in one mode only.

-- Some data is usefully render info but should be specified by
-- library writer not by the end user, e.g. global_do_beaming
--
-- Potentially there is a relationship between a user exposed 
-- config and a larger internal one like Parsec\'s LanguageDef 
-- and TokenParser



data ScoreInfo = ScoreInfo
    { global_temp_abc_file      :: !String
    , global_temp_ly_file       :: !String
    , global_title              :: !String
    , global_ly_octave_mode     :: !OctaveMode
    , global_ly_version         :: !String
    , global_clef               :: !Clef
    }
  deriving (Data,Eq,Show,Typeable)

data OctaveMode = AbsPitch 
                | RelPitch !Pitch
  deriving (Data,Eq,Show,Typeable)

data Clef = TREBLE | BASS
  deriving (Data,Eq,Show,Typeable)


default_score_info :: ScoreInfo
default_score_info = ScoreInfo
    { global_temp_abc_file      = "abc_output.abc"
    , global_temp_ly_file       = "output.ly"
    , global_title              = ""
    , global_ly_octave_mode     = RelPitch middle_c
    , global_ly_version         = "2.18.2"
    , global_clef               = TREBLE
    }


-- | Note - @LocalRenderInfo@ is stored as a header to a Bar in
-- Beam and Main syntax.
-- 
-- This allows concatenating bars together (and tempo and key 
-- changes). Generally outputting must be sensitive to changes 
-- to LocalRenderInfo as a new bar is printed.
-- 
-- Mono syntax is more suitable to transformation, hence we put 
-- @LocalRenderInfo@ at the start of the phrase
--

data LocalContextInfo = LocalContextInfo
    { local_key                 :: !Key
    , local_meter               :: !Meter
    , local_meter_patn          :: !MeterPattern
    , local_unit_note_len       :: !UnitNoteLength
    , local_bpm                 :: !BPM
    }
  deriving (Data,Eq,Show,Typeable)

data Key = Key !PitchName !Mode
  deriving (Data,Eq,Ord,Show,Typeable)


data Mode = MAJOR | MINOR | MIXOLYDIAN | DORIAN | PHRYGIAN | LYDIAN | LOCRIAN
  deriving (Data,Enum,Eq,Ord,Show,Typeable)

-- | CommonTime = 4/4
--   CutTime = 2/4
--
-- TODO - add free metered.
--
data Meter = Meter Int Int
  deriving (Data,Eq,Ord,Show,Typeable)

-- TODO - span bars with meter pattern (e.g. for 2-bar patterns
-- as in South American music).
--
type MeterPattern = [RDuration]

data UnitNoteLength = UNIT_NOTE_4 | UNIT_NOTE_8 | UNIT_NOTE_16
  deriving (Data,Enum,Eq,Ord,Show,Typeable)







default_local_info :: LocalContextInfo
default_local_info = LocalContextInfo 
    { local_key                 = c_maj
    , local_meter               = Meter 4 4 
    , local_meter_patn          = [1%2,1%2]
    , local_unit_note_len       = UNIT_NOTE_8
    , local_bpm                 = 120
    }






barLength :: Meter -> RDuration
barLength (Meter n d) = (fromIntegral n) * fn d
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
