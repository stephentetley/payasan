{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Names.GeneralMidiDrums
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Named instances of General MIDI drums.
--
-- Use channel 9 to get percussion sounds
--
--------------------------------------------------------------------------------

module Payasan.Base.Names.GeneralMidiDrums
  (

  -- * General MIDI drums
    acoustic_bass_drum
  , bass_drum_1
  , side_stick
  , acoustic_snare
  , hand_clap
  , electric_snare
  , low_floor_tom
  , closed_hi_hat
  , high_floor_tom
  , pedal_hi_hat
  , low_tom
  , open_hi_hat
  , low_mid_tom
  , high_mid_tom
  , crash_cymbal_1
  , high_tom
  , ride_cymbal_1
  , chinese_cymbal
  , ride_bell
  , tambourine
  , splash_cymbal
  , cowbell
  , crash_cymbal_2
  , vibraslap
  , ride_cymbal_2
  , high_bongo
  , low_bongo
  , mute_high_conga
  , open_high_conga
  , low_conga
  , high_timbale
  , low_timbale
  , high_agogo
  , low_agogo
  , cabasa
  , maracas
  , short_whistle
  , long_whistle
  , short_guiro
  , long_guiro
  , claves
  , high_wood_block
  , low_wood_block
  , mute_cuica
  , open_cuica
  , mute_triangle
  , open_triangle

  ) where

import Payasan.Base.AltPitch

-------------------------------------------------------------------------------
-- GM drums

acoustic_bass_drum      :: MidiPitch
acoustic_bass_drum      = MidiPitch 35

bass_drum_1             :: MidiPitch
bass_drum_1             = MidiPitch 36

side_stick              :: MidiPitch
side_stick              = MidiPitch 37

acoustic_snare          :: MidiPitch
acoustic_snare          = MidiPitch 38

hand_clap               :: MidiPitch
hand_clap               = MidiPitch 39

electric_snare          :: MidiPitch
electric_snare          = MidiPitch 40

low_floor_tom           :: MidiPitch
low_floor_tom           = MidiPitch 41

closed_hi_hat           :: MidiPitch
closed_hi_hat           = MidiPitch 42

high_floor_tom          :: MidiPitch
high_floor_tom          = MidiPitch 43

pedal_hi_hat            :: MidiPitch
pedal_hi_hat            = MidiPitch 44

low_tom                 :: MidiPitch
low_tom                 = MidiPitch 45

open_hi_hat             :: MidiPitch
open_hi_hat             = MidiPitch 46

low_mid_tom             :: MidiPitch
low_mid_tom             = MidiPitch 47

high_mid_tom            :: MidiPitch
high_mid_tom            = MidiPitch 48

crash_cymbal_1          :: MidiPitch
crash_cymbal_1          = MidiPitch 49

high_tom                :: MidiPitch
high_tom                = MidiPitch 50

ride_cymbal_1           :: MidiPitch
ride_cymbal_1           = MidiPitch 51

chinese_cymbal          :: MidiPitch
chinese_cymbal          = MidiPitch 52

ride_bell               :: MidiPitch
ride_bell               = MidiPitch 53

tambourine              :: MidiPitch
tambourine              = MidiPitch 54

splash_cymbal           :: MidiPitch
splash_cymbal           = MidiPitch 55

cowbell                 :: MidiPitch
cowbell                 = MidiPitch 56

crash_cymbal_2          :: MidiPitch
crash_cymbal_2          = MidiPitch 57

vibraslap               :: MidiPitch
vibraslap               = MidiPitch 58

ride_cymbal_2           :: MidiPitch
ride_cymbal_2           = MidiPitch 59

high_bongo              :: MidiPitch
high_bongo              = MidiPitch 60

low_bongo               :: MidiPitch
low_bongo               = MidiPitch 61

mute_high_conga         :: MidiPitch
mute_high_conga         = MidiPitch 62

open_high_conga         :: MidiPitch
open_high_conga         = MidiPitch 63

low_conga               :: MidiPitch
low_conga               = MidiPitch 64

high_timbale            :: MidiPitch
high_timbale            = MidiPitch 65

low_timbale             :: MidiPitch
low_timbale             = MidiPitch 66

high_agogo              :: MidiPitch
high_agogo              = MidiPitch 67

low_agogo               :: MidiPitch
low_agogo               = MidiPitch 68

cabasa                  :: MidiPitch
cabasa                  = MidiPitch 69

maracas                 :: MidiPitch
maracas                 = MidiPitch 70

short_whistle           :: MidiPitch
short_whistle           = MidiPitch 71

long_whistle            :: MidiPitch
long_whistle            = MidiPitch 72

short_guiro             :: MidiPitch
short_guiro             = MidiPitch 73

long_guiro              :: MidiPitch
long_guiro              = MidiPitch 74

claves                  :: MidiPitch
claves                  = MidiPitch 75

high_wood_block         :: MidiPitch
high_wood_block         = MidiPitch 76

low_wood_block          :: MidiPitch
low_wood_block          = MidiPitch 77

mute_cuica              :: MidiPitch
mute_cuica              = MidiPitch 78

open_cuica              :: MidiPitch
open_cuica              = MidiPitch 79

mute_triangle           :: MidiPitch
mute_triangle           = MidiPitch 80

open_triangle           :: MidiPitch
open_triangle           = MidiPitch 81

