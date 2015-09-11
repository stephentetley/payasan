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

    GlobalRenderInfo(..)
  , PitchDirective(..)

  , default_global_info

  , LocalRenderInfo(..)
  , UnitNoteLength(..)
  , KeySig(..)
  , Mode(..)
  , TimeSig(..)

  , default_local_info

  , barLength

  , MeterPattern
  , TupletSpec(..)



  -- * Keys
  , c_maj

  ) where

import Payasan.Base.Internal.Base

import Payasan.Base.Duration
import Payasan.Base.Pitch


import Data.Data
import Data.Ratio


-- | Note don\'t store LilyPond Absolute / Relative pitch
-- at bar level. This a a global property as we must render
-- in one mode only.

data GlobalRenderInfo = GlobalRenderInfo
    { global_temp_file_prefix   :: !String
    , global_title              :: !String
    , global_pitch_directive    :: !PitchDirective
    }
  deriving (Data,Eq,Show,Typeable)

data PitchDirective = AbsPitch 
                    | RelPitch !Pitch
  deriving (Data,Eq,Show,Typeable)

default_global_info :: GlobalRenderInfo
default_global_info = GlobalRenderInfo
    { global_temp_file_prefix   = "output"
    , global_title              = ""
    , global_pitch_directive    = RelPitch middle_c
    }


-- | Note - @LocalRenderInfo@ is stored as a header to a Bar. 
-- This allows concatenating bars together. Generally outputting 
-- should be sensitive to changes to LocalRenderInfo as a new bar
-- is printed.
-- 
 
data LocalRenderInfo = LocalRenderInfo
    { local_key_sig             :: !KeySig
    , local_time_sig            :: !TimeSig
    , local_meter_patn          :: !MeterPattern
    , local_unit_note_len       :: !UnitNoteLength
    , local_bpm                 :: !BPM
    }
  deriving (Data,Eq,Show,Typeable)

data UnitNoteLength = UNIT_NOTE_4 | UNIT_NOTE_8 | UNIT_NOTE_16
  deriving (Data,Enum,Eq,Ord,Show,Typeable)


data KeySig = KeySig !PitchSpelling !Mode
  deriving (Data,Eq,Ord,Show,Typeable)


data Mode = MAJOR | MINOR | MIXOLYDIAN | DORIAN | PHRYGIAN | LYDIAN | LOCRIAN
  deriving (Data,Enum,Eq,Ord,Show,Typeable)

-- | CommonTime = 4/4
--   CutTime = 2/4
--
-- TODO - add free metered.
--
data TimeSig = Meter Int Int
  deriving (Data,Eq,Ord,Show,Typeable)

type MeterPattern = [RDuration]




default_local_info :: LocalRenderInfo
default_local_info = LocalRenderInfo 
    { local_key_sig             = c_maj
    , local_time_sig            = Meter 4 4 
    , local_meter_patn          = [1%2,1%2]
    , local_unit_note_len       = UNIT_NOTE_8
    , local_bpm                 = 120
    }






barLength :: TimeSig -> RDuration
barLength (Meter n d) = (fromIntegral n) * fn d
  where
    fn i = 1 % fromIntegral i



data TupletSpec = TupletSpec
     { tuplet_num   :: Int
     , tuplet_time  :: Int
     , tuplet_len   :: Int
     }
  deriving (Data,Eq,Show,Typeable)






c_maj :: KeySig
c_maj = KeySig (PitchSpelling C NAT) MAJOR
