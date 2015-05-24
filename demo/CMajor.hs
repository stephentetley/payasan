{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module NoteList01 where


import Payasan.Base
import Payasan.Base.Names.GeneralMidiInstruments
import Payasan.Models.Melody

import Payasan.Base.Names.Pitch


demo01 :: IO ()
demo01 = writeMF0 "out/cmajor.mid" $ renderMelodyPhrase pianoNote (mkTconfig acoustic_grand_piano) $ 
    localize (set_bpm 140) $ melodyPhrase melody01



melody01 :: Melody
melody01 = [melody| M:4/4 \\ C D E F G A B c  c d e f g a b c' |]


mkTconfig :: Int -> TrackData
mkTconfig pc = TrackData { channel_number    = 1
                         , program_change    = Just pc
                         , generic_text      = ""
                         , sequence_name     = ""
                         , instrument_name   = ""
                         }



pianoNote :: MelodyF
pianoNote = makeMelodyF $ \pch ->
    makeEvent () (NoteValue { note_pitch    = pitchToMidi pch
                            , note_velo_on  = 65
                            , note_velo_off = 65
                            })



