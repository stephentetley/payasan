{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.MIDI.Syntax
-- Copyright   :  (c) Stephen Tetley 2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- IRBeamFlat specialized for MIDI.
-- 
--------------------------------------------------------------------------------

module Payasan.PSC.MIDI.Syntax
  ( 

    MIDITicks
  , MIDIPart
  , MIDISection
  , MIDIEvent
  , MIDIEventBody(..)

  , secondsToMIDITicks

  ) where


import Payasan.PSC.Base.Utils
import Payasan.PSC.Repr.IREventFlat.Syntax

import Payasan.Base.AltPitch
import Payasan.Base.Basis (Seconds)

import Data.Function ( on )
import Data.Word

type MIDITicks = Word32


type MIDIPart    = Part     MIDITicks () MIDIEventBody

type MIDISection = Section  MIDITicks () MIDIEventBody

type MIDIEvent   = Event    MIDITicks () MIDIEventBody


data MIDIEventBody = 
      NoteOff  { midi_note :: !MidiPitch, midi_velocity :: !Word8 }
    | NoteOn   { midi_note :: !MidiPitch, midi_velocity :: !Word8 }
  deriving (Eq,Ord,Show)





secondsToMIDITicks :: Int -> Seconds -> MIDITicks
secondsToMIDITicks tpqn r = floor $ (4 * ticks_per_quarternote) * (realToFrac r)
  where
    ticks_per_quarternote :: Double
    ticks_per_quarternote = realToFrac tpqn





