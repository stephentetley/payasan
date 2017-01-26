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

    AbsTicks
  , DeltaTicks

  , MIDIPart
  , MIDISection
  , MIDIEvent
  , MIDIEventBody(..)

  , secondsToMIDITicks
  , ticksTrafo

  ) where


import Payasan.PSC.Repr.IREventFlat.Syntax
import Payasan.PSC.Repr.IREventFlat.Traversals
import Payasan.PSC.Base.Utils

import Payasan.Base.AltPitch
import Payasan.Base.Basis (Seconds)

import Data.Function ( on )
import Data.Word

type AbsTicks = Word32
type DeltaTicks = Word32


type MIDIPart onset     = Part     onset () MIDIEventBody

type MIDISection onset  = Section  onset () MIDIEventBody

type MIDIEvent onset    = Event    onset () MIDIEventBody


data MIDIEventBody = 
      NoteOff  { midi_note :: !MidiPitch, midi_velocity :: !Word8 }
    | NoteOn   { midi_note :: !MidiPitch, midi_velocity :: !Word8 }
  deriving (Eq,Ord,Show)





secondsToMIDITicks :: Int -> Seconds -> AbsTicks
secondsToMIDITicks tpqn r = floor $ (4 * ticks_per_quarternote) * (realToFrac r)
  where
    ticks_per_quarternote :: Double
    ticks_per_quarternote = realToFrac tpqn




ticksTrafo :: Int -> Part Seconds () MIDIEventBody -> MIDIPart AbsTicks
ticksTrafo tpqn = mapOnset (secondsToMIDITicks tpqn)

