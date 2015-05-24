{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module Chord01 where


import Payasan.Base
import Payasan.Base.Names.Chord
import Payasan.Base.Names.GeneralMidiInstruments
import Payasan.Base.Names.Pitch

import Payasan.Models.ChordProgression




demo01 :: IO ()
demo01 = writeMF0 "out/vibes_chords01.mid" $ renderChordPhrase vibesNote (mkTconfig vibraphone) $ 
    localize (set_bpm 180) $ mreplicate 4 $ chordPhrase chords01

{-
demo02 :: IO ()
demo02 = writeMF0 "out/accordion_chords01.mid" $ renderSection accordionNote (mkTconfig accordion) $ 
    localize (set_bpm 180) $ mreplicate 4 $ section phrase01
-}

chords01 :: ChordProgression
chords01 = makeChordProgression [ R 1, N (chordNotes $ major_triad c_4) 1 ]


mkTconfig :: Int -> TrackData
mkTconfig pc = TrackData { channel_number    = 1
                         , program_change    = Just pc
                         , generic_text      = ""
                         , sequence_name     = ""
                         , instrument_name   = ""
                         }



vibesNote :: ChordF
vibesNote = makeChordF $ \pch ->
    makeEventFw () 2.0 (NoteValue { note_pitch    = pitchToMidi pch
                                  , note_velo_on  = 65
                                  , note_velo_off = 65
                                  })


accordionNote :: ChordF 
accordionNote = makeChordF $ \pch ->
    makeEvent () (NoteValue { note_pitch    = pitchToMidi pch
                            , note_velo_on  = 65
                            , note_velo_off = 65
                            })



