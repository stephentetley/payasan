{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Backend.MIDI.PrimitiveSyntax
-- Copyright   :  (c) Stephen Tetley 2014-2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Midi syntax types.
-- 
-- Note type (includes duration) - note on and note off are
-- derived from MidiNote.
-- 
--------------------------------------------------------------------------------

module Payasan.PSC.Backend.MIDI.PrimitiveSyntax
  ( 

  -- * syntax
    Track(..)
  , InterimTrack(..)
  , TrackData(..)
  , MidiNote(..)
  , NoteValue(..)
  , MidiNoteList
  , MidiPitch   -- re-export

  -- * Helpers
  , simpleTrackData
  , compareMidiNote
  , pitchToMidi

  ) where


import Payasan.Base.Basis
import Payasan.Base.Utils

import Payasan.Base.Pitch


import Data.Function ( on )



newtype Track = Track { getTrack :: InterimTrack }
 


data InterimTrack = InterimTrack
    { track_config      :: TrackData
    , track_notes       :: [MidiNote]
    } 
  deriving (Show)

data TrackData = TrackData
    { channel_number    :: Int
    , program_change    :: Maybe Int
    , generic_text      :: String
    , sequence_name     :: String
    , instrument_name   :: String
    }
  deriving (Show) 


data MidiNote = MidiNote
    { note_start        :: !Seconds
    , note_dur          :: !Seconds
    , note_value        :: !NoteValue
    }
  deriving (Eq,Show)

data NoteValue = NoteValue
    { note_pitch        :: !MidiPitch
    , note_velo_on      :: !Int
    , note_velo_off     :: !Int
    }
  deriving (Eq,Show)


-- We want statements at a concrete type that allows snocing.
--
--
type MidiNoteList = H MidiNote





--------------------------------------------------------------------------------
-- Helpers

simpleTrackData :: Int -> TrackData
simpleTrackData ch = TrackData
    { channel_number    = ch
    , program_change    = Nothing
    , generic_text      = ""
    , sequence_name     = ""
    , instrument_name   = ""
    }

-- | Sort by instrument, then start time.
compareMidiNote :: MidiNote -> MidiNote -> Ordering
compareMidiNote = compare `on` note_start



pitchToMidi :: Pitch -> MidiPitch
pitchToMidi = fromIntegral . midiSemitoneCount








