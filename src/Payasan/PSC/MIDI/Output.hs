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

{-
    render
  , midiFileFormat0
  , midiFileFormat1
  , writeMF0
  , writeMF1
-}
  ) where

import Payasan.PSC.MIDI.Syntax
import Payasan.PSC.Repr.IREventFlat.Syntax
import Payasan.PSC.Repr.IREventFlat.Traversals
import Payasan.PSC.Base.Utils

import Payasan.Base.AltPitch (MidiPitch(..), getMidiPitch)
import Payasan.Base.Basis (Seconds)

import qualified ZMidi.Core as Z                -- package: zmidi-core

import Data.List ( sort )
import Data.Word



{-

-- Note - this is a Double so it is the right type for calculating
-- durations, in MIDI files the size of the ticks-per-beat 
-- designator is actually a Word16.
--
ticks_per_quarternote :: Double
ticks_per_quarternote = 480.0

miditime :: Seconds -> Word32
miditime r = floor $ (4 * ticks_per_quarternote) * (realToFrac r)

delta_end_of_track :: Z.MidiMessage
delta_end_of_track = (0, Z.MetaEvent $ Z.EndOfTrack)


-- render should make a Track...
render :: Track -> Z.MidiTrack
render = renderInterim . getTrack


midiFileFormat0 :: Track -> Z.MidiFile
midiFileFormat0 trk = Z.MidiFile { Z.mf_header = header
                                 , Z.mf_tracks = [ render trk ] }
  where
    header  :: Z.MidiHeader
    header  = Z.MidiHeader { Z.hdr_format    = Z.MF0
                           , Z.num_tracks    = 1
                           , Z.time_division = timediv }

    timediv = Z.TPB $ floor $ ticks_per_quarternote



midiFileFormat1 :: [Track] -> Z.MidiFile
midiFileFormat1 trks = Z.MidiFile { Z.mf_header = header
                                  , Z.mf_tracks = map render trks }
  where
    header  :: Z.MidiHeader
    header  = Z.MidiHeader { Z.hdr_format    = Z.MF1
                           , Z.num_tracks    = fromIntegral $ length trks
                           , Z.time_division = timediv }

    timediv = Z.TPB $ floor $ ticks_per_quarternote

writeMF0 :: FilePath -> Track -> IO ()
writeMF0 path trk = Z.writeMidi path $ midiFileFormat0 trk

writeMF1 :: FilePath -> [Track] -> IO ()
writeMF1 path trks = Z.writeMidi path $ midiFileFormat1 trks


--------------------------------------------------------------------------------
-- Interim to ZMidi

renderInterim :: InterimTrack -> Z.MidiTrack
renderInterim (InterimTrack cfg xs) = Z.MidiTrack $ prolog $ body
  where
    prolog  = trackPrologue cfg
    body    = messageList (fromIntegral $ channel_number cfg) xs



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



-- Maybe MIDIPart should actually store onset in Seconds
-- and we encapsulate the unit change to (delta) MidiTicks 
-- in this module?


renderTrack :: Word8 -> MIDIPart AbsTicks -> Z.MidiTrack
renderTrack chan p = Z.MidiTrack $ part chan (deltaTrafo p)

delta_end_of_track :: Z.MidiMessage
delta_end_of_track = (0, Z.MetaEvent $ Z.EndOfTrack)


part :: Word8 -> MIDIPart DeltaTicks -> [Z.MidiMessage]
part chan (Part { part_sections = ss }) = go ss
  where
    go []     = [delta_end_of_track]
    go (x:xs) = section chan x ++ go xs



section :: Word8 -> MIDISection DeltaTicks -> [Z.MidiMessage]
section chan (Section { section_events = es }) = map (event chan) es


-- Onset has already been transformed to delta time...
-- 
event :: Word8 -> MIDIEvent DeltaTicks -> Z.MidiMessage
event chan (Event { event_onset = ot, event_body = body }) = 
    (fromIntegral ot, Z.VoiceEvent Z.RS_OFF $ eventBody chan body)


eventBody :: Word8 -> MIDIEventBody -> Z.MidiVoiceEvent
eventBody chan (NoteOff pch vel) = Z.NoteOff  chan (getMidiPitch pch) vel
eventBody chan (NoteOn  pch vel) = Z.NoteOn   chan (getMidiPitch pch) vel



--------------------------------------------------------------------------------
-- Delta trafo

deltaTrafo :: MIDIPart AbsTicks -> MIDIPart DeltaTicks
deltaTrafo = mapAccumOnset (\acc ot -> (ot, ot - acc)) 0
