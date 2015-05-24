{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module Djembe01 where


import Payasan.Base
import Payasan.Base.Names.GeneralMidiDrums
import Payasan.Models.Djembe


import Prelude hiding ( replicate )


-- Use channel 9 for General MIDI percussion
--
tconfig :: TrackData
tconfig = TrackData { channel_number   = 9
                    , program_change   = Nothing
                    , generic_text     = ""
                    , sequence_name    = ""
                    , instrument_name  = ""
                    }


-- | tempo????
--
-- Hiding Advance may have been too hasty as it has good properties 
-- (concat, tempo evaluation...)
--
demo01 :: IO ()
demo01 = writeMF0 "out/djembe01.mid" $ renderDjembePhrase strokeF tconfig $ mreplicate 10 $ djembePhrase $
    [djembe| flam{tone tone} tone tone // tone tone . // tone tone . // tone . . |]


-- accented strokes
demo02 :: DjembePattern
demo02 = [djembe| >slap . tone tone // >slap . tone tone // tone >slap . >slap // . . tone tone |]

-- swing
demo03 :: IO ()
demo03 = writeMF0 "out/djembe03.mid" $ renderDjembePhrase strokeF tconfig $ mreplicate 10 $ djembePhrase $ 
    [djembe| tone swing{slap} tone // swing{tone} tone swing{slap} // slap swing{tone} tone // swing{slap} slap swing{tone} |]

-- use angles for chords...
demo04 :: DjembePattern
demo04 = [djembe| <tone slap> . slap . // <tone slap> . <tone slap> . // <tone slap> . slap . // <tone slap> . slap . |]


-- Interpreter...
--

strokeF :: StrokeF
strokeF ACCENT     (Identifier "tone") = tone 127
strokeF NO_ACCENT  (Identifier "tone") = tone 65
strokeF ACCENT     (Identifier "slap") = slap 127 
strokeF NO_ACCENT  (Identifier "slap") = slap 65
strokeF _          (Identifier ss)     = error $ "unidentified symbol: " ++ ss

tone :: Int -> Event ()
tone onn = 
    makeEventFw () 0.5 (NoteValue { note_pitch    = low_tom
                                  , note_velo_on  = onn
                                  , note_velo_off = 65
                                  })

slap :: Int -> Event ()
slap onn = 
    makeEventFw () 0.5 (NoteValue { note_pitch    = side_stick
                                  , note_velo_on  = onn
                                  , note_velo_off = 65
                                  })

