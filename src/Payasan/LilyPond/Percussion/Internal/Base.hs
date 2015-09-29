{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.Percussion.Internal.Base
-- Copyright   :  (c Stephen Tetley 2015
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
   
    LyDrumPhrase
  , StdDrumPhrase

  , Accent(..)
  , DrumPitch(..)
  , shortName
  , toMidiPitch

  ) where

import qualified Payasan.Base.Internal.BeamSyntax as BEAM
import qualified Payasan.Base.Internal.MainSyntax as MAIN

import Payasan.Base.Internal.LilyPond.Syntax (LyNoteLength)
import Payasan.Base.Internal.MIDI.PrimitiveSyntax (MidiPitch)

import Payasan.Base.Duration

import Text.PrettyPrint.HughesPJClass           -- package: pretty

import Data.Data


type LyDrumPhrase       = BEAM.Phrase DrumPitch LyNoteLength Accent
type StdDrumPhrase      = MAIN.Phrase DrumPitch Duration     Accent

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
toMidiPitch :: DrumPitch -> MidiPitch
toMidiPitch Acousticbassdrum    = 35            -- MIDI B0
toMidiPitch Bassdrum            = 36            -- MIDI C1
toMidiPitch Hisidestick         = 37            -- use Sidestick
toMidiPitch Sidestick           = 37            -- MIDI C#1
toMidiPitch Losidestick         = 37            -- use Sidestick
toMidiPitch Acousticsnare       = 38            -- MIDI D1
toMidiPitch Snare               = 38            -- use Acousticksnare
toMidiPitch Handclap            = 39            -- MIDI D#1
toMidiPitch Electricsnare       = 40            -- MIDI E1
toMidiPitch Lowfloortom         = 41            -- MIDI F1
toMidiPitch Closedhihat         = 42            -- MIDI F#1
toMidiPitch Hihat               = 42            -- use Closedhihat
toMidiPitch Highfloortom        = 43            -- MIDI G1
toMidiPitch Pedalhihat          = 44            -- MIDI G#1
toMidiPitch Lowtom              = 45            -- MIDI A1
toMidiPitch Openhihat           = 46            -- MIDI A#1
toMidiPitch Halfopenhihat       = 46            -- use Halfopenhihat
toMidiPitch Lowmidtom           = 47            -- MIDI B1
toMidiPitch Himidtom            = 48            -- MIDI C2
toMidiPitch Crashcymbala        = 49            -- MIDI C#2
toMidiPitch Crashcymbal         = 49            -- use Crashcymbala
toMidiPitch Hightom             = 50            -- MIDI D2
toMidiPitch Ridecymbala         = 51            -- MIDI D#2
toMidiPitch Ridecymbal          = 51            -- use Ridecymbala
toMidiPitch Chinesecymbal       = 52            -- MIDI E2
toMidiPitch Ridebell            = 53            -- MIDI F2
toMidiPitch Tambourine          = 54            -- MIDI F#2
toMidiPitch Splashcymbal        = 55            -- MIDI G2
toMidiPitch Cowbell             = 56            -- MIDI G#2
toMidiPitch Crashcymbalb        = 57            -- MIDI A2
toMidiPitch Vibraslap           = 58            -- MIDI A#2
toMidiPitch Ridecymbalb         = 59            -- MIDI B2
toMidiPitch Mutehibongo         = 60            -- use Hibongo
toMidiPitch Hibongo             = 60            -- MIDI C3
toMidiPitch Openhibongo         = 60            -- use Hibongo
toMidiPitch Mutelobongo         = 61            -- use Lobongo
toMidiPitch Lobongo             = 61            -- MIDI C#3
toMidiPitch Openlobongo         = 61            -- use Lobongo
toMidiPitch Mutehiconga         = 62            -- MIDI D3
toMidiPitch Muteloconga         = 62            -- use Muteloconga
toMidiPitch Openhiconga         = 63            -- MIDI D#3
toMidiPitch Hiconga             = 63            -- use Openhiconga
toMidiPitch Openloconga         = 64            -- use Loconga
toMidiPitch Loconga             = 64            -- MIDI E3
toMidiPitch Hitimbale           = 65            -- MIDI F3
toMidiPitch Lotimbale           = 66            -- MIDI F#3
toMidiPitch Hiagogo             = 67            -- MIDI G3
toMidiPitch Loagogo             = 68            -- MIDI G#3
toMidiPitch Cabasa              = 69            -- MIDI A3
toMidiPitch Maracas             = 70            -- MIDI A#3
toMidiPitch Shortwhistle        = 71            -- MIDI B3
toMidiPitch Longwhistle         = 72            -- MIDI C4
toMidiPitch Shortguiro          = 73            -- MIDI C#4
toMidiPitch Longguiro           = 74            -- MIDI D4
toMidiPitch Guiro               = 74            -- Use Longguiro
toMidiPitch Claves              = 75            -- MIDI D#4
toMidiPitch Hiwoodblock         = 76            -- MIDI E4
toMidiPitch Lowoodblock         = 77            -- MIDI F4
toMidiPitch Mutecuica           = 78            -- MIDI F#4
toMidiPitch Opencuica           = 79            -- MIDI G4
toMidiPitch Mutetriangle        = 80            -- MIDI A#4
toMidiPitch Triangle            = 80            -- Use Mutetriangle
toMidiPitch Opentriangle        = 81            -- MIDI B4
toMidiPitch Oneup               = 62            -- same as Mutehiconga
toMidiPitch Twoup               = 64            -- same as Loconga
toMidiPitch Threeup             = 65            -- same as Hitambale 
toMidiPitch Fourup              = 67            -- same as Hiagogo
toMidiPitch Fiveup              = 69            -- same as Cabasa
toMidiPitch Onedown             = 59            -- same as Ridecymbalb
toMidiPitch Twodown             = 57            -- same as CrashCymbalb
toMidiPitch Threedown           = 55            -- same as Splashcymbal
toMidiPitch Fourdown            = 53            -- same as Ridebell
toMidiPitch Fivedown            = 52            -- same as Chinesecymbal 



instance Pretty DrumPitch where
  pPrint = text . shortName