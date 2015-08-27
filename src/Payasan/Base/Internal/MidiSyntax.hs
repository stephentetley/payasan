{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.MidiSyntax
-- Copyright   :  (c) Stephen Tetley 2014-2015
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

module Payasan.Base.Internal.MidiSyntax
  ( 

  -- * syntax
    Track(..)
  , InterimTrack(..)
  , TrackData(..)
  , MidiNote(..)
  , NoteValue(..)
  , MidiNoteList

  -- * Helpers
  , compareMidiNote

  ) where


import Payasan.Base.Internal.Base
import Payasan.Base.Internal.Utils

import qualified ZMidi.Core as Z

import Data.Function ( on )

newtype Track = Track { getTrack :: Z.MidiTrack }
 


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



newtype MidiPitch = MidiPitch Int
  deriving (Enum,Eq,Ord,Num,Real,Integral,Show)


--------------------------------------------------------------------------------
-- Helpers



-- | Sort by instrument, then start time.
compareMidiNote :: MidiNote -> MidiNote -> Ordering
compareMidiNote = compare `on` note_start

{-

pitchToMidi :: Pitch -> MidiPitch
pitchToMidi (Pitch o i) = fromIntegral $ fromIntegral i + ove
  where
    ove = 12 * (o-3)
-}









