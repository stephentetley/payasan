{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module NoteList01 where


import Payasan.Base
import Payasan.Base.Names.GeneralMidiInstruments
import Payasan.Symbolic.SimpleNoteList



demo01 :: IO ()
demo01 = writeMF0 "out/vibes01.mid" $ renderPhrase vibesNote (mkTconfig vibraphone) $ 
    localize (set_bpm 180) $ mreplicate 4 $ phrase motif01


demo02 :: IO ()
demo02 = writeMF0 "out/accordion01.mid" $ renderPhrase accordionNote (mkTconfig accordion) $ 
    localize (set_bpm 180) $ mreplicate 4 $ phrase motif01


motif01 :: Motif
motif01 = [motif| c G2 E2 C/2 |]


mkTconfig :: Int -> TrackData
mkTconfig pc = TrackData { channel_number    = 1
                         , program_change    = Just pc
                         , generic_text      = ""
                         , sequence_name     = ""
                         , instrument_name   = ""
                         }



vibesNote :: Pitch -> Event ()
vibesNote pch =  
    makeEventFw () 2.0 (NoteValue { note_pitch    = pitchToMidi pch
                                  , note_velo_on  = 65
                                  , note_velo_off = 65
                                  })


accordionNote :: Pitch -> Event ()
accordionNote pch = 
    makeEvent () (NoteValue { note_pitch    = pitchToMidi pch
                            , note_velo_on  = 65
                            , note_velo_off = 65
                            })



