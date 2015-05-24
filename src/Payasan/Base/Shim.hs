{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Shim
-- Copyright   :  (c) Stephen Tetley 2014
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Import shim module to use Base internals.
-- 
--------------------------------------------------------------------------------

module Payasan.Base.Shim
  (

    module Payasan.Base.Internal.Base


  -- * Payasan.Base.Internal.MidiOutput
  , render
  , midiFileFormat0
  , midiFileFormat1
  , writeMF0
  , writeMF1

  -- * Payasan.Base.Internal.Midi
  , Track
  , TrackData(..)
  , MidiNote(..)
  , NoteValue(..)

  -- * Payasan.Base.Internal.Pitch
  , Pitch
  , makePitch
  , flatten
  , sharpen
  , addSemitones

  , midiToPitch
  , pitchToMidi

  , MidiPitch
  , Interval

  , Chord(..)
  , ChordIntervals
  , chordNotes
  , chordF
  , chordAdd
  , chordDelete

  ) where

import Payasan.Base.Internal.Base
import Payasan.Base.Internal.Midi
import Payasan.Base.Internal.MidiOutput
import Payasan.Base.Internal.Pitch


