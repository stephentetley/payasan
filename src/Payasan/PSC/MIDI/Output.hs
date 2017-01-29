{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.MIDI.Output
-- Copyright   :  (c) Stephen Tetley 2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Generate MIDI files from MIDI IRFlatSyntax.
-- 
--------------------------------------------------------------------------------

module Payasan.PSC.MIDI.Output
  ( 

    makeZMidiTrack

  , midiFileFormat0
  , midiFileFormat1 
  ) where

import Payasan.PSC.MIDI.Syntax
import Payasan.PSC.Repr.IREventFlat.Syntax

import Payasan.Base.AltPitch (MidiPitch(..), getMidiPitch)

import qualified ZMidi.Core as Z                -- package: zmidi-core

import Data.List ( sort, mapAccumL )







makeZMidiTrack :: MIDIEventList -> Z.MidiTrack
makeZMidiTrack = render . preprocess . getMIDIEventList 
  where
    preprocess  = deltaTrafo . sort
    render xs   = Z.MidiTrack $ step xs
    step []     = [delta_end_of_track]
    step (e:es) = event e : step es


delta_end_of_track :: Z.MidiMessage
delta_end_of_track = (0, Z.MetaEvent $ Z.EndOfTrack)


-- Onset has already been transformed to delta time...
-- 
event :: MIDIEvent DeltaTicks -> Z.MidiMessage
event (Event { event_onset = ot, event_body = body }) = 
    (fromIntegral ot, Z.VoiceEvent Z.RS_OFF $ eventBody body)


eventBody :: MIDIEventBody -> Z.MidiVoiceEvent
eventBody (NoteOff chan pch vel)    = Z.NoteOff  chan (getMidiPitch pch) vel
eventBody (NoteOn  chan pch vel)    = Z.NoteOn   chan (getMidiPitch pch) vel



deltaTrafo :: [MIDIEvent AbsTicks] -> [MIDIEvent DeltaTicks]
deltaTrafo = snd .  mapAccumL fn 0
  where
    fn acc evt@(Event { event_onset = ot }) = (ot, evt { event_onset = ot - acc })



--------------------------------------------------------------------------------
-- Make MIDI files


midiFileFormat0 :: Int -> Z.MidiTrack -> Z.MidiFile
midiFileFormat0 tpqn trk = 
    Z.MidiFile { Z.mf_header = header
               , Z.mf_tracks = [ trk ] }
  where
    header  :: Z.MidiHeader
    header  = Z.MidiHeader { Z.hdr_format    = Z.MF0
                           , Z.num_tracks    = 1
                           , Z.time_division = Z.TPB $ fromIntegral tpqn }

 



midiFileFormat1 :: Int -> [Z.MidiTrack] -> Z.MidiFile
midiFileFormat1 tpqn trks = 
    Z.MidiFile { Z.mf_header = header
               , Z.mf_tracks = trks }
  where
    header  :: Z.MidiHeader
    header  = Z.MidiHeader { Z.hdr_format    = Z.MF1
                           , Z.num_tracks    = fromIntegral $ length trks
                           , Z.time_division = Z.TPB $ fromIntegral tpqn }



{-

trackPrologue :: TrackData -> H Z.MidiMessage
trackPrologue (TrackData { channel_number  = ch
                         , program_change  = mb_inst
                         , generic_text    = text
                         , sequence_name   = sname
                         , instrument_name = iname }) = t1 . t2 . t3 . t4
  where
    build s ty | null s    = id 
               | otherwise = wrapH $ (0, Z.MetaEvent $ Z.TextEvent ty s)
    t1         = build text  Z.GENERIC_TEXT 
    t2         = build sname Z.SEQUENCE_NAME
    t3         = build iname Z.INSTRUMENT_NAME
    t4         = case mb_inst of 
                   Nothing -> id
                   Just i -> let pc  = Z.ProgramChange (fromIntegral ch) (fromIntegral i)
                                 evt = Z.VoiceEvent Z.RS_OFF pc
                             in wrapH (0, evt) 


-}


