{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.MIDI.Output
-- Copyright   :  (c) Stephen Tetley 2014-2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Generate MIDI files.
-- 
--------------------------------------------------------------------------------

module Payasan.Base.Internal.MIDI.Output
  ( 
    render
  , midiFileFormat0
  , midiFileFormat1
  , writeMF0
  , writeMF1

  ) where

import Payasan.Base.Internal.MIDI.Syntax

import Payasan.Base.Internal.Base
import Payasan.Base.Internal.Utils

import qualified ZMidi.Core as Z                -- packahe: zmidi-core

import Data.List ( sort )
import Data.Word



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
render :: InterimTrack -> Track
render = Track . renderInterim


midiFileFormat0 :: Track -> Z.MidiFile
midiFileFormat0 trk = Z.MidiFile { Z.mf_header = header
                           , Z.mf_tracks = [ getTrack trk ] }
  where
    header  :: Z.MidiHeader
    header  = Z.MidiHeader { Z.hdr_format    = Z.MF0
                           , Z.num_tracks    = 1
                           , Z.time_division = timediv }

    timediv = Z.TPB $ floor $ ticks_per_quarternote



midiFileFormat1 :: [Track] -> Z.MidiFile
midiFileFormat1 trks = Z.MidiFile { Z.mf_header = header
                            , Z.mf_tracks = map getTrack trks }
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


-- Assumes single instrument - should channel be part of MidiNote?
messageList :: Word8 -> [MidiNote] -> [Z.MidiMessage]
messageList chan = delta 0 . sort . expand
  where
    expand []              = []
    expand (x:xs)          = let (a,b) = expandMidiNote chan x in a:b:expand xs

    delta dt ((ot,msg):xs) = let ot1 = fromIntegral $ miditime ot 
                             in (ot1 - dt,msg) : delta ot1 xs
    delta _  []            = [delta_end_of_track]



type AbsMessage = (Seconds,Z.MidiEvent)

expandMidiNote :: Word8 -> MidiNote -> (AbsMessage, AbsMessage)
expandMidiNote chan (MidiNote { note_start    = ot
                              , note_dur      = drn
                              , note_value    = val }) = 
    let pch   = fromIntegral $ note_pitch val
        von   = fromIntegral $ note_velo_on val
        voff  = fromIntegral $ note_velo_off val
        onn   = Z.VoiceEvent Z.RS_OFF (Z.NoteOn chan pch von)
        off   = Z.VoiceEvent Z.RS_OFF (Z.NoteOff chan pch voff)
    in ((ot,onn), (ot+drn,off))
