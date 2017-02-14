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

  , MIDIEventList(..)

  , MIDIPart
  , MIDISection
  , MIDIEvent
  , MIDIEventBody(..)

  , ticks_per_quarter_note
  , secondsToMIDITicks
  , ticksTrafo

  , makeMIDIEventList
  , mergeMIDIEventLists

  ) where


import Payasan.PSC.Repr.IREventFlat.Syntax
import Payasan.PSC.Repr.IREventFlat.Traversals

import Payasan.Base.AltPitch
import Payasan.Base.Basis (Seconds)

import Data.Word

type AbsTicks = Word32
type DeltaTicks = Word32

-- We need an equivalent of LyNoteListDoc to assemble multiple 
-- parts together for the final rendering.
-- 
newtype MIDIEventList = MIDIEventList { getMIDIEventList :: [MIDIEvent AbsTicks] }


type MIDIPart onset     = Part     onset () MIDIEventBody

type MIDISection onset  = Section  onset () MIDIEventBody

type MIDIEvent onset    = Event    onset () MIDIEventBody


data MIDIEventBody = 
      NoteOff  !Word8 !MidiPitch !Word8         -- chan * pch * velocity
    | NoteOn   !Word8 !MidiPitch !Word8         
  deriving (Eq,Ord,Show)



--------------------------------------------------------------------------------
-- Operations


ticks_per_quarter_note :: Int
ticks_per_quarter_note = 480


secondsToMIDITicks :: Seconds -> AbsTicks
secondsToMIDITicks r = floor $ (4 * tpqn) * (realToFrac r)
  where
    tpqn :: Double
    tpqn = realToFrac ticks_per_quarter_note



ticksTrafo :: Part Seconds () MIDIEventBody -> MIDIPart AbsTicks
ticksTrafo = mapOnset secondsToMIDITicks


-- No end of track
makeMIDIEventList :: MIDIPart AbsTicks -> MIDIEventList
makeMIDIEventList (Part { part_sections = ss }) = 
    MIDIEventList $ concatMap section_events ss

mergeMIDIEventLists :: [MIDIEventList] -> MIDIEventList
mergeMIDIEventLists xs = MIDIEventList $ foldr fn [] xs
  where
    fn x ac = ac ++ getMIDIEventList x



