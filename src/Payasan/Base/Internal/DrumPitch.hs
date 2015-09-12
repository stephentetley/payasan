{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.DrumPitch
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

module Payasan.Base.Internal.DrumPitch
  ( 
    DrumPitch(..)
  , shortName
  , toMidiPitch
  ) where

import Payasan.Base.Internal.MIDI.Syntax

import Data.Data

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

toMidiPitch :: DrumPitch -> MidiPitch
toMidiPitch Acousticbassdrum    = 35
toMidiPitch Bassdrum            = 36
toMidiPitch Hisidestick         = 0
toMidiPitch Sidestick           = 37
toMidiPitch Losidestick         = 0
toMidiPitch Acousticsnare       = 38
toMidiPitch Snare               = 0
toMidiPitch Handclap            = 39
toMidiPitch Electricsnare       = 40
toMidiPitch Lowfloortom         = 41
toMidiPitch Closedhihat         = 42
toMidiPitch Hihat               = 0
toMidiPitch Highfloortom        = 43
toMidiPitch Pedalhihat          = 44
toMidiPitch Lowtom              = 45
toMidiPitch Openhihat           = 46
toMidiPitch Halfopenhihat       = 0
toMidiPitch Lowmidtom           = 47
toMidiPitch Himidtom            = 48
toMidiPitch Crashcymbala        = 49
toMidiPitch Crashcymbal         = 49
toMidiPitch Hightom             = 50
toMidiPitch Ridecymbala         = 51
toMidiPitch Ridecymbal          = 51
toMidiPitch Chinesecymbal       = 52
toMidiPitch Ridebell            = 53
toMidiPitch Tambourine          = 54
toMidiPitch Splashcymbal        = 55
toMidiPitch Cowbell             = 56
toMidiPitch Crashcymbalb        = 57
toMidiPitch Vibraslap           = 58
toMidiPitch Ridecymbalb         = 59
toMidiPitch Mutehibongo         = 0
toMidiPitch Hibongo             = 60
toMidiPitch Openhibongo         = 0
toMidiPitch Mutelobongo         = 0
toMidiPitch Lobongo             = 61
toMidiPitch Openlobongo         = 0
toMidiPitch Mutehiconga         = 62
toMidiPitch Muteloconga         = 0
toMidiPitch Openhiconga         = 63
toMidiPitch Hiconga             = 0
toMidiPitch Openloconga         = 0
toMidiPitch Loconga             = 64
toMidiPitch Hitimbale           = 65
toMidiPitch Lotimbale           = 66
toMidiPitch Hiagogo             = 67
toMidiPitch Loagogo             = 68
toMidiPitch Cabasa              = 69
toMidiPitch Maracas             = 70
toMidiPitch Shortwhistle        = 71
toMidiPitch Longwhistle         = 72
toMidiPitch Shortguiro          = 73
toMidiPitch Longguiro           = 74
toMidiPitch Guiro               = 0
toMidiPitch Claves              = 75
toMidiPitch Hiwoodblock         = 76
toMidiPitch Lowoodblock         = 77
toMidiPitch Mutecuica           = 78
toMidiPitch Opencuica           = 79 
toMidiPitch Mutetriangle        = 80
toMidiPitch Triangle            = 0
toMidiPitch Opentriangle        = 81
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



