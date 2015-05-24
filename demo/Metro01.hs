{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module Metro01 where


import Payasan.Base
import Payasan.Base.Names.GeneralMidiDrums

import Payasan.Models.Metronome


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

m1 :: Metronome
m1 = metro (4,8)


-- | Where should tickF be /injected/ ?
--
-- Ideally we would want a single instance, but it needs to be
-- /pushed to the leaves/ in order to render fragments...
--
demo01 :: IO ()
demo01 = writeMF0 "out/metro01.mid" $ renderMetroPhrase tickF tconfig $ 
    localize (set_bpm 180) $ mreplicate 32 $ metroPhrase m1


tickF :: TickF
tickF STRONG_TICK = tone 127
tickF TICK        = slap 65

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
