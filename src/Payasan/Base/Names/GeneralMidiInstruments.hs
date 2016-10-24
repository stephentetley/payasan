{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Names.GeneralMidiInstruments
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Named instances of General MIDI instruments.
--
--------------------------------------------------------------------------------

module Payasan.Base.Names.GeneralMidiInstruments
  (

  -- * General MIDI instruments

    acoustic_grand_piano
  , bright_acoustic_piano
  , electric_grand_piano
  , honky_tonk
  , electric_piano_1
  , electric_piano_2
  , harpsichord
  , clavicord

    -- ** Chromatic persussion
  , celesta
  , glockenspiel
  , music_box
  , vibraphone
  , marimba
  , xylophone
  , tubular_bells
  , dulcimer

  -- ** Organ
  , drawbar_organ
  , percussive_organ
  , rock_organ
  , church_organ
  , reel_organ
  , accordion
  , harmonica
  , tango_accordian

  -- ** Guitar
  , nylon_acoustic_guitar
  , steel_acoustic_guitar
  , jazz_electric_guitar
  , clean_electric_guitar
  , muted_electric_guitar
  , overdriven_guitar
  , distortion_guitar
  , guitar_harmonics

  -- ** Bass
  , acoustic_bass
  , finger_electric_bass
  , pick_electric_bass
  , fretless_bass
  , slap_bass_1
  , slap_bass_2
  , synth_bass_1
  , synth_bass_2

  -- ** Strings
  , violin
  , viola
  , cello
  , contrabass
  , tremolo_strings
  , pizzicato_strings
  , orchestral_strings
  , timpani

  -- ** Ensemble
  , string_ensemble_1
  , string_ensemble_2
  , synth_strings_1
  , synth_strings_2
  , choir_aahs
  , voice_oohs
  , synth_voice
  , orchestra_hit


  -- ** Brass
  , trumpet
  , trombone
  , tuba
  , muted_trumpet
  , french_horn
  , brass_section
  , synth_brass_1
  , synth_brass_2

  -- ** Reed
  , soprano_sax
  , alto_sax
  , tenor_sax
  , baritone_sax
  , oboe
  , english_horn
  , bassoon
  , clarinet

  -- ** Pipe
  , piccolo
  , flute
  , recorder
  , pan_flute
  , blown_bottle
  , shakuhachi
  , whistle
  , ocarina

  -- ** Synth lead
  , square_lead
  , sawtooth_lead
  , calliope_lead
  , chiff_lead
  , charang_lead
  , voice_lead
  , fifths_lead
  , bass_lead

  -- ** Synth pad
  , new_age_pad
  , warm_pad
  , polysynth_pad
  , choir_pad
  , bowed_pad
  , metallic_pad
  , halo_pad
  , sweep_pad

  -- ** Synth effects
  , rain
  , soundtrack
  , crystal
  , atmosphere
  , brightness
  , goblins
  , echoes
  , sci_fi

  -- ** World
  , sitar
  , banjo
  , shamisen
  , koto
  , kalimba
  , bagpipe
  , fiddle
  , shanai

  -- ** Percussive
  , tingle_bell
  , agogo
  , steel_drums
  , woodblock
  , taiko_drum
  , melodic_tom
  , synth_drum
  , reverse_cymbal


  -- ** Sound effects
  , guitar_fret_noise
  , breath_noise
  , seashore
  , bird_tweet
  , telephone_ring
  , helicopter
  , applause
  , gunshot


  ) where





--------------------------------------------------------------------------------
-- GM insts

-- Piano

acoustic_grand_piano    :: Int
acoustic_grand_piano    = 0

bright_acoustic_piano   :: Int
bright_acoustic_piano   = 1

electric_grand_piano    :: Int
electric_grand_piano    = 2

honky_tonk              :: Int
honky_tonk              = 3

electric_piano_1        :: Int
electric_piano_1        = 4

electric_piano_2        :: Int
electric_piano_2        = 5

harpsichord             :: Int
harpsichord             = 6

clavicord               :: Int
clavicord               = 7


-- Chromatic persussion

celesta                 :: Int
celesta                 = 8

glockenspiel            :: Int
glockenspiel            = 9

music_box               :: Int
music_box               = 10

vibraphone              :: Int
vibraphone              = 11

marimba                 :: Int
marimba                 = 12

xylophone               :: Int
xylophone               = 13

tubular_bells           :: Int
tubular_bells           = 14

dulcimer                :: Int
dulcimer                = 15


-- Organ

drawbar_organ           :: Int
drawbar_organ           = 16

percussive_organ        :: Int
percussive_organ        = 17

rock_organ              :: Int
rock_organ              = 18

church_organ            :: Int
church_organ            = 19

reel_organ              :: Int
reel_organ              = 20

accordion               :: Int
accordion               = 21

harmonica               :: Int
harmonica               = 22

tango_accordian         :: Int
tango_accordian         = 23


-- Guitar

nylon_acoustic_guitar   :: Int
nylon_acoustic_guitar   = 24

steel_acoustic_guitar   :: Int
steel_acoustic_guitar   = 25

jazz_electric_guitar    :: Int
jazz_electric_guitar    = 26

clean_electric_guitar   :: Int
clean_electric_guitar   = 27

muted_electric_guitar   :: Int
muted_electric_guitar   = 28

overdriven_guitar       :: Int
overdriven_guitar       = 29

distortion_guitar       :: Int
distortion_guitar       = 30

guitar_harmonics        :: Int
guitar_harmonics        = 31


-- Bass

acoustic_bass           :: Int
acoustic_bass           = 32

finger_electric_bass    :: Int
finger_electric_bass    = 33

pick_electric_bass      :: Int
pick_electric_bass      = 34

fretless_bass           :: Int
fretless_bass           = 35

slap_bass_1             :: Int
slap_bass_1             = 36
                        
slap_bass_2             :: Int
slap_bass_2             = 37

synth_bass_1            :: Int
synth_bass_1            = 38

synth_bass_2            :: Int
synth_bass_2            = 39


-- Strings

violin                  :: Int
violin                  = 40

viola                   :: Int
viola                   = 41

cello                   :: Int
cello                   = 42

contrabass              :: Int
contrabass              = 43

tremolo_strings         :: Int
tremolo_strings         = 44

pizzicato_strings       :: Int
pizzicato_strings       = 45

orchestral_strings      :: Int
orchestral_strings      = 46

timpani                 :: Int
timpani                 = 47


--  Ensemble

string_ensemble_1       :: Int
string_ensemble_1       = 48

string_ensemble_2       :: Int
string_ensemble_2       = 49

synth_strings_1         :: Int
synth_strings_1         = 50

synth_strings_2         :: Int
synth_strings_2         = 51

choir_aahs              :: Int
choir_aahs              = 52

voice_oohs              :: Int
voice_oohs              = 53

synth_voice             :: Int
synth_voice             = 54

orchestra_hit           :: Int
orchestra_hit           = 55


-- Brass

trumpet                 :: Int
trumpet                 = 56

trombone                :: Int
trombone                = 57

tuba                    :: Int
tuba                    = 58

muted_trumpet           :: Int
muted_trumpet           = 59

french_horn             :: Int
french_horn             = 60

brass_section           :: Int
brass_section           = 61

synth_brass_1           :: Int
synth_brass_1           = 62

synth_brass_2           :: Int
synth_brass_2           = 63


-- Reed

soprano_sax             :: Int
soprano_sax             = 64

alto_sax                :: Int
alto_sax                = 65

tenor_sax               :: Int
tenor_sax               = 66

baritone_sax            :: Int
baritone_sax            = 67

oboe                    :: Int
oboe                    = 68

english_horn            :: Int
english_horn            = 69

bassoon                 :: Int
bassoon                 = 70

clarinet                :: Int
clarinet                = 71

-- Pipe

piccolo                 :: Int
piccolo                 = 72

flute                   :: Int
flute                   = 73

recorder                :: Int
recorder                = 74

pan_flute               :: Int
pan_flute               = 75

blown_bottle            :: Int
blown_bottle            = 76

shakuhachi              :: Int
shakuhachi              = 77

whistle                 :: Int
whistle                 = 78

ocarina                 :: Int
ocarina                 = 79


-- Synth lead

square_lead             :: Int
square_lead             = 80

sawtooth_lead           :: Int
sawtooth_lead           = 81

calliope_lead           :: Int
calliope_lead           = 82

chiff_lead              :: Int
chiff_lead              = 83

charang_lead            :: Int
charang_lead            = 84

voice_lead              :: Int
voice_lead              = 85

fifths_lead             :: Int
fifths_lead             = 86

bass_lead               :: Int
bass_lead               = 87


-- Synth pad

new_age_pad             :: Int
new_age_pad             = 88

warm_pad                :: Int
warm_pad                = 89

polysynth_pad           :: Int
polysynth_pad           = 90

choir_pad               :: Int
choir_pad               = 91

bowed_pad               :: Int
bowed_pad               = 92

metallic_pad            :: Int
metallic_pad            = 93

halo_pad                :: Int
halo_pad                = 94

sweep_pad               :: Int
sweep_pad               = 95


-- Synth effects

rain                    :: Int
rain                    = 96

soundtrack              :: Int
soundtrack              = 97

crystal                 :: Int
crystal                 = 98

atmosphere              :: Int
atmosphere              = 99

brightness              :: Int
brightness              = 100

goblins                 :: Int
goblins                 = 101

echoes                  :: Int
echoes                  = 102

sci_fi                  :: Int
sci_fi                  = 103


-- World

sitar                   :: Int
sitar                   = 104

banjo                   :: Int
banjo                   = 105

shamisen                :: Int
shamisen                = 106

koto                    :: Int
koto                    = 107

kalimba                 :: Int
kalimba                 = 108

bagpipe                 :: Int
bagpipe                 = 109

fiddle                  :: Int
fiddle                  = 110

shanai                  :: Int
shanai                  = 111


-- Percussive

tingle_bell             :: Int
tingle_bell             = 112

agogo                   :: Int
agogo                   = 113

steel_drums             :: Int
steel_drums             = 114

woodblock               :: Int
woodblock               = 115

taiko_drum              :: Int
taiko_drum              = 116

melodic_tom             :: Int
melodic_tom             = 117

synth_drum              :: Int
synth_drum              = 118

reverse_cymbal          :: Int
reverse_cymbal          = 119


-- Sound effects

guitar_fret_noise       :: Int
guitar_fret_noise       = 120

breath_noise            :: Int
breath_noise            = 121

seashore                :: Int
seashore                = 122

bird_tweet              :: Int
bird_tweet              = 123

telephone_ring          :: Int
telephone_ring          = 124

helicopter              :: Int
helicopter              = 125

applause                :: Int
applause                = 126

gunshot                 :: Int
gunshot                 = 127

