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
    RenderInfo(..)
  , UnitNoteLength(..)
  , PitchDirective(..)
  , default_render_info

  , KeySig(..)
  , Mode(..)
  , TimeSig(..)
  , MeterPattern
  , TupletSpec(..)

  -- * Pretty printing
  , abcMode
  , abcKeySig
  , abcTimeSig


  -- * Keys
  , c_maj

  ) where

import Payasan.Base.Internal.Base

import Payasan.Base.Duration
import Payasan.Base.Pitch

import Text.PrettyPrint.HughesPJ hiding ( Mode )       -- package: pretty

import Data.Data
import Data.Ratio



data RenderInfo = RenderInfo
    { render_key_sig            :: !KeySig
    , render_time_sig           :: !TimeSig
    , render_meter_patn         :: !MeterPattern
    , render_unit_note_len      :: !UnitNoteLength
    , render_bpm                :: !BPM
    , render_ly_pitch           :: !PitchDirective
    }
  deriving (Data,Eq,Show,Typeable)

data UnitNoteLength = UNIT_NOTE_4 | UNIT_NOTE_8 | UNIT_NOTE_16
  deriving (Data,Enum,Eq,Ord,Show,Typeable)

data PitchDirective = AbsPitch | RelPitch Pitch
  deriving (Data,Eq,Show,Typeable)

default_render_info :: RenderInfo
default_render_info = RenderInfo 
    { render_key_sig            = c_maj
    , render_time_sig           = Meter 4 4 
    , render_meter_patn         = [1%2,1%2]
    , render_unit_note_len      = UNIT_NOTE_8
    , render_bpm                = 120
    , render_ly_pitch           = RelPitch middle_c
    }


data KeySig = KeySig  NoteLabel Mode
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



data TupletSpec = TupletSpec
     { tuplet_num   :: Int
     , tuplet_time  :: Int
     , tuplet_len   :: Int
     }
  deriving (Data,Eq,Show,Typeable)




abcMode :: Mode -> Doc
abcMode MAJOR       = empty
abcMode MINOR       = text "m"
abcMode MIXOLYDIAN  = text "Mix"
abcMode DORIAN      = text "Dor"
abcMode PHRYGIAN    = text "Phr"
abcMode LYDIAN      = text "Lyd"
abcMode LOCRIAN     = text "Loc"


abcKeySig :: KeySig -> Doc
abcKeySig (KeySig n m)  = 
    let (l,a) = abcElements n in text l <> text a <> abcMode m


abcElements :: NoteLabel -> (String,String)
abcElements (NoteLabel l a) = (show l, altname a) 
  where
    altname DBL_FLAT    = "bb"
    altname FLAT        = "b"
    altname NAT         = ""
    altname SHARP       = "#"
    altname DBL_SHARP   = "##"


abcTimeSig :: TimeSig -> Doc
abcTimeSig (Meter n d)  = int n <> char '/' <> int d



c_maj :: KeySig
c_maj = KeySig (NoteLabel C NAT) MAJOR
