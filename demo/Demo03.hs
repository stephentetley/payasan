{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module Demo03 where


import Payasan.Base.Duration
import Payasan.Base.Notelist
import Payasan.Base.Pitch


phrase01 :: StdPhrase
phrase01 = fromLilyPondWith global_ri manual_ri $ 
    [lilypond| c4 d e fis | c |]


global_ri :: GlobalRenderInfo
global_ri = GlobalRenderInfo
    { global_pitch_directive = RelPitch middle_c
    }


manual_ri :: LocalRenderInfo
manual_ri = default_local_info { local_unit_note_len = UNIT_NOTE_4 }


demo01 :: IO ()
demo01 = printAsABC phrase01

