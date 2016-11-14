{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.Percussion.Internal.Base
-- Copyright   :  (c) Stephen Tetley 2015-2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Drum pitches for LilyPond and MIDI.
--
--------------------------------------------------------------------------------

module Payasan.LilyPond.Percussion.Internal.Base
  ( 
   
    StdDrumPart
  , LyDrumPart

  , Accent(..)
  , DrumPitch(..)
  , shortName
  , toMidiValue

  ) where

import Payasan.PSC.Base.LilyPondCommon (LyNoteLength)

import qualified Payasan.PSC.Repr.External.Syntax       as EXT



import Payasan.Base.AltPitch
import Payasan.Base.Duration
import qualified Payasan.Base.Names.GeneralMidiDrums    as GM

import Text.PrettyPrint.HughesPJClass           -- package: pretty

import Data.Data


type StdDrumPart        = EXT.Part  DrumPitch Duration     Accent
type LyDrumPart         = EXT.Part  DrumPitch LyNoteLength Accent

data Accent = ACCENT | NO_ACCENT
  deriving (Data,Eq,Ord,Show,Typeable)

data DrumPitch = Acousticbassdrum
               | Bassdrum
               | Hisidestick
               | Sidestick
               | Losidestick
               | Acousticsnare
               | Snare
               | Handclap
               | Electricsnare
               | Lowfloortom
               | Closedhihat
               | Hihat
               | Highfloortom
               | Pedalhihat
               | Lowtom
               | Openhihat
               | Halfopenhihat
               | Lowmidtom
               | Himidtom
               | Crashcymbala
               | Crashcymbal
               | Hightom
               | Ridecymbala
               | Ridecymbal
               | Chinesecymbal
               | Ridebell
               | Tambourine
               | Splashcymbal
               | Cowbell
               | Crashcymbalb
               | Vibraslap
               | Ridecymbalb
               | Mutehibongo
               | Hibongo
               | Openhibongo
               | Mutelobongo
               | Lobongo
               | Openlobongo
               | Mutehiconga
               | Muteloconga
               | Openhiconga
               | Hiconga
               | Openloconga
               | Loconga
               | Hitimbale
               | Lotimbale
               | Hiagogo
               | Loagogo
               | Cabasa
               | Maracas
               | Shortwhistle
               | Longwhistle
               | Shortguiro
               | Longguiro
               | Guiro
               | Claves
               | Hiwoodblock
               | Lowoodblock
               | Mutecuica
               | Opencuica
               | Mutetriangle
               | Triangle
               | Opentriangle
               | Oneup
               | Twoup
               | Threeup
               | Fourup
               | Fiveup
               | Onedown
               | Twodown
               | Threedown
               | Fourdown
               | Fivedown
  deriving (Data,Eq,Ord,Show,Typeable)


-- | Short name for LilyPond output.
--
shortName :: DrumPitch -> String
shortName Acousticbassdrum      = "bda"
shortName Bassdrum              = "bd"
shortName Hisidestick           = "ssh" 
shortName Sidestick             = "ss"
shortName Losidestick           = "ssl"
shortName Acousticsnare         = "sna"
shortName Snare                 = "sn"
shortName Handclap              = "hc" 
shortName Electricsnare         = "sne"
shortName Lowfloortom           = "tomfl"
shortName Closedhihat           = "hhc" 
shortName Hihat                 = "hh"
shortName Highfloortom          = "tomfh"
shortName Pedalhihat            = "hhp"
shortName Lowtom                = "toml"
shortName Openhihat             = "hho"
shortName Halfopenhihat         = "hhho"
shortName Lowmidtom             = "tomml"
shortName Himidtom              = "tommh"
shortName Crashcymbala          = "cymca"
shortName Crashcymbal           = "cymc"
shortName Hightom               = "tomh"
shortName Ridecymbala           = "cymra"
shortName Ridecymbal            = "cymr"
shortName Chinesecymbal         = "cymch"
shortName Ridebell              = "rb"
shortName Tambourine            = "tamb"
shortName Splashcymbal          = "cyms"
shortName Cowbell               = "cb"
shortName Crashcymbalb          = "cymcb"
shortName Vibraslap             = "vibs"
shortName Ridecymbalb           = "cymrb"
shortName Mutehibongo           = "bohm"
shortName Hibongo               = "boh"
shortName Openhibongo           = "boho"
shortName Mutelobongo           = "bolm"
shortName Lobongo               = "bol"
shortName Openlobongo           = "bolo"
shortName Mutehiconga           = "cghm"
shortName Muteloconga           = "cglm"
shortName Openhiconga           = "cgho"
shortName Hiconga               = "cgh"
shortName Openloconga           = "cglo"
shortName Loconga               = "cgl"
shortName Hitimbale             = "timh"
shortName Lotimbale             = "timl"
shortName Hiagogo               = "agh"
shortName Loagogo               = "agl"
shortName Cabasa                = "cab"
shortName Maracas               = "mar"
shortName Shortwhistle          = "whs"
shortName Longwhistle           = "whl"
shortName Shortguiro            = "guis" 
shortName Longguiro             = "guil" 
shortName Guiro                 = "gui"
shortName Claves                = "cl" 
shortName Hiwoodblock           = "wbh" 
shortName Lowoodblock           = "wbl" 
shortName Mutecuica             = "cuim" 
shortName Opencuica             = "cuio" 
shortName Mutetriangle          = "trim" 
shortName Triangle              = "tri" 
shortName Opentriangle          = "trio" 
shortName Oneup                 = "ua" 
shortName Twoup                 = "ub" 
shortName Threeup               = "uc" 
shortName Fourup                = "ud"
shortName Fiveup                = "ue" 
shortName Onedown               = "da" 
shortName Twodown               = "db" 
shortName Threedown             = "dc" 
shortName Fourdown              = "dd" 
shortName Fivedown              = "de" 



-- | Note - LilyPond has more durm pitches than MIDI and
-- there does not always seem to be a correspondence when 
-- they match.
--
toMidiValue :: DrumPitch -> MidiPitch
toMidiValue Acousticbassdrum    = GM.acoustic_bass_drum         -- MIDI B0
toMidiValue Bassdrum            = GM.bass_drum_1                -- MIDI C1
toMidiValue Hisidestick         = GM.side_stick                 -- use Sidestick
toMidiValue Sidestick           = GM.side_stick                 -- MIDI C#1
toMidiValue Losidestick         = GM.side_stick                 -- use Sidestick
toMidiValue Acousticsnare       = GM.acoustic_snare             -- MIDI D1
toMidiValue Snare               = GM.acoustic_snare             -- use Acousticksnare
toMidiValue Handclap            = GM.hand_clap                  -- MIDI D#1
toMidiValue Electricsnare       = GM.electric_snare             -- MIDI E1
toMidiValue Lowfloortom         = GM.low_floor_tom              -- MIDI F1
toMidiValue Closedhihat         = GM.closed_hi_hat              -- MIDI F#1
toMidiValue Hihat               = GM.closed_hi_hat              -- use Closedhihat
toMidiValue Highfloortom        = GM.high_floor_tom             -- MIDI G1
toMidiValue Pedalhihat          = GM.pedal_hi_hat               -- MIDI G#1
toMidiValue Lowtom              = GM.low_tom                    -- MIDI A1
toMidiValue Openhihat           = GM.open_hi_hat                -- MIDI A#1
toMidiValue Halfopenhihat       = GM.open_hi_hat                -- use Openhihat
toMidiValue Lowmidtom           = GM.low_mid_tom                -- MIDI B1
toMidiValue Himidtom            = GM.high_mid_tom               -- MIDI C2
toMidiValue Crashcymbala        = GM.crash_cymbal_1             -- MIDI C#2
toMidiValue Crashcymbal         = GM.crash_cymbal_1             -- use Crashcymbala
toMidiValue Hightom             = GM.high_tom                   -- MIDI D2
toMidiValue Ridecymbala         = GM.ride_cymbal_1              -- MIDI D#2
toMidiValue Ridecymbal          = GM.ride_cymbal_1              -- use Ridecymbala
toMidiValue Chinesecymbal       = GM.chinese_cymbal             -- MIDI E2
toMidiValue Ridebell            = GM.ride_bell                  -- MIDI F2
toMidiValue Tambourine          = GM.tambourine                 -- MIDI F#2
toMidiValue Splashcymbal        = GM.splash_cymbal              -- MIDI G2
toMidiValue Cowbell             = GM.cowbell                    -- MIDI G#2
toMidiValue Crashcymbalb        = GM.crash_cymbal_2             -- MIDI A2
toMidiValue Vibraslap           = GM.vibraslap                  -- MIDI A#2
toMidiValue Ridecymbalb         = GM.ride_cymbal_2              -- MIDI B2
toMidiValue Mutehibongo         = GM.high_bongo                 -- use Hibongo
toMidiValue Hibongo             = GM.high_bongo                 -- MIDI C3
toMidiValue Openhibongo         = GM.high_bongo                 -- use Hibongo
toMidiValue Mutelobongo         = GM.low_bongo                  -- use Lobongo
toMidiValue Lobongo             = GM.low_bongo                  -- MIDI C#3
toMidiValue Openlobongo         = GM.low_bongo                  -- use Lobongo
toMidiValue Mutehiconga         = GM.mute_high_conga            -- MIDI D3
toMidiValue Muteloconga         = GM.low_conga                  -- useLoconga
toMidiValue Openhiconga         = GM.open_high_conga            -- MIDI D#3
toMidiValue Hiconga             = GM.open_high_conga            -- use Openhiconga
toMidiValue Openloconga         = GM.low_conga                  -- use Loconga
toMidiValue Loconga             = GM.low_conga                  -- MIDI E3
toMidiValue Hitimbale           = GM.high_timbale               -- MIDI F3
toMidiValue Lotimbale           = GM.low_timbale                -- MIDI F#3
toMidiValue Hiagogo             = GM.high_agogo                 -- MIDI G3
toMidiValue Loagogo             = GM.low_agogo                  -- MIDI G#3
toMidiValue Cabasa              = GM.cabasa                     -- MIDI A3
toMidiValue Maracas             = GM.maracas                    -- MIDI A#3
toMidiValue Shortwhistle        = GM.short_whistle              -- MIDI B3
toMidiValue Longwhistle         = GM.long_whistle               -- MIDI C4
toMidiValue Shortguiro          = GM.short_guiro                -- MIDI C#4
toMidiValue Longguiro           = GM.long_guiro                 -- MIDI D4
toMidiValue Guiro               = GM.long_guiro                 -- Use Longguiro
toMidiValue Claves              = GM.claves                     -- MIDI D#4
toMidiValue Hiwoodblock         = GM.high_wood_block            -- MIDI E4
toMidiValue Lowoodblock         = GM.low_wood_block             -- MIDI F4
toMidiValue Mutecuica           = GM.mute_cuica                 -- MIDI F#4
toMidiValue Opencuica           = GM.open_cuica                 -- MIDI G4
toMidiValue Mutetriangle        = GM.mute_triangle              -- MIDI A#4
toMidiValue Triangle            = GM.open_triangle              -- Use Opentriangle
toMidiValue Opentriangle        = GM.open_triangle              -- MIDI B4
toMidiValue Oneup               = GM.mute_high_conga            -- same as Mutehiconga
toMidiValue Twoup               = GM.low_conga                  -- same as Loconga
toMidiValue Threeup             = GM.high_timbale               -- same as Hitambale 
toMidiValue Fourup              = GM.high_agogo                 -- same as Hiagogo
toMidiValue Fiveup              = GM.cabasa                     -- same as Cabasa
toMidiValue Onedown             = GM.ride_cymbal_2              -- same as Ridecymbalb
toMidiValue Twodown             = GM.crash_cymbal_2             -- same as CrashCymbalb
toMidiValue Threedown           = GM.splash_cymbal              -- same as Splashcymbal
toMidiValue Fourdown            = GM.ride_bell                  -- same as Ridebell
toMidiValue Fivedown            = GM.chinese_cymbal             -- same as Chinesecymbal 



instance Pretty DrumPitch where
  pPrint = text . shortName