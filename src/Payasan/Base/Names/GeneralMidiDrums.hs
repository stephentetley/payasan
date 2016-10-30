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

-- TODO - imports in Payasan.Base should not look outside...
import Payasan.PSC.Backend.MIDI.PrimitiveSyntax

-------------------------------------------------------------------------------
-- GM drums

acoustic_bass_drum      :: MidiPitch
acoustic_bass_drum      = 35

bass_drum_1             :: MidiPitch
bass_drum_1             = 36

side_stick              :: MidiPitch
side_stick              = 37

acoustic_snare          :: MidiPitch
acoustic_snare          = 38

hand_clap               :: MidiPitch
hand_clap               = 39

electric_snare          :: MidiPitch
electric_snare          = 40

low_floor_tom           :: MidiPitch
low_floor_tom           = 41

closed_hi_hat           :: MidiPitch
closed_hi_hat           = 42

high_floor_tom          :: MidiPitch
high_floor_tom          = 43

pedal_hi_hat            :: MidiPitch
pedal_hi_hat            = 44

low_tom                 :: MidiPitch
low_tom                 = 45

open_hi_hat             :: MidiPitch
open_hi_hat             = 46

low_mid_tom             :: MidiPitch
low_mid_tom             = 47

high_mid_tom            :: MidiPitch
high_mid_tom            = 48

crash_cymbal_1          :: MidiPitch
crash_cymbal_1          = 49

high_tom                :: MidiPitch
high_tom                = 50

ride_cymbal_1           :: MidiPitch
ride_cymbal_1           = 51

chinese_cymbal          :: MidiPitch
chinese_cymbal          = 52

ride_bell               :: MidiPitch
ride_bell               = 53

tambourine              :: MidiPitch
tambourine              = 54

splash_cymbal           :: MidiPitch
splash_cymbal           = 55

cowbell                 :: MidiPitch
cowbell                 = 56

crash_cymbal_2          :: MidiPitch
crash_cymbal_2          = 57

vibraslap               :: MidiPitch
vibraslap               = 58

ride_cymbal_2           :: MidiPitch
ride_cymbal_2           = 59

high_bongo              :: MidiPitch
high_bongo              = 60

low_bongo               :: MidiPitch
low_bongo               = 61

mute_high_conga         :: MidiPitch
mute_high_conga         = 62

open_high_conga         :: MidiPitch
open_high_conga         = 63

low_conga               :: MidiPitch
low_conga               = 64

high_timbale            :: MidiPitch
high_timbale            = 65

low_timbale             :: MidiPitch
low_timbale             = 66

high_agogo              :: MidiPitch
high_agogo              = 67

low_agogo               :: MidiPitch
low_agogo               = 68

cabasa                  :: MidiPitch
cabasa                  = 69

maracas                 :: MidiPitch
maracas                 = 70

short_whistle           :: MidiPitch
short_whistle           = 71

long_whistle            :: MidiPitch
long_whistle            = 72

short_guiro             :: MidiPitch
short_guiro             = 73

long_guiro              :: MidiPitch
long_guiro              = 74

claves                  :: MidiPitch
claves                  = 75

high_wood_block         :: MidiPitch
high_wood_block         = 76

low_wood_block          :: MidiPitch
low_wood_block          = 77

mute_cuica              :: MidiPitch
mute_cuica              = 78

open_cuica              :: MidiPitch
open_cuica              = 79

mute_triangle           :: MidiPitch
mute_triangle           = 80

open_triangle           :: MidiPitch
open_triangle           = 81

