{-# OPTIONS -Wall #-}

module Demo01 where


import Payasan.Base
import Payasan.Base.EventList

import Payasan.Base.Names.Pitch


demo01 :: IO ()
demo01 = writeMF0 "out01.mid" eventlist

wgBowedBar :: Pitch -> Event ()
wgBowedBar pch = 
    makeEventFw () 0.5 (NoteValue { note_pitch    = pitchToMidi pch
                                  , note_velo_on  = 65
                                  , note_velo_off = 65
                                  })

eventlist :: Track
eventlist = renderEventList track_cfg $ do
                { event    0   0.25 $ wgBowedBar c_4
                ; event    0.5 2    $ wgBowedBar e_4
                ; accTrill 2   1 e_4 e_4 4 $ \pch -> wgBowedBar pch
                ; trill    3   2 c_4 e_4 4 $ \pch -> wgBowedBar pch
                ; decTrill 6   2 e_4 e_4 4 $ \pch -> wgBowedBar pch
                }




track_cfg :: TrackData
track_cfg = TrackData { channel_number    = 1
                      , program_change    = Nothing
                      , generic_text      = ""
                      , sequence_name     = ""
                      , instrument_name   = ""
                      }


