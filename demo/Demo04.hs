{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module Demo04 where

import Payasan.Base.Monophonic.Notelist

import Payasan.Base.Duration
import Payasan.Base.Pitch
import Payasan.Base.Names.Interval


-- INTERVALS -- 

phrase01 :: StdMonoPhrase
phrase01 = fromLilyPondWith global_ri manual_ri $ 
    [lilypond| d2 a | f d |]



global_ri :: GlobalRenderInfo
global_ri = GlobalRenderInfo
    { global_pitch_directive = RelPitch middle_c
    }


manual_ri :: LocalRenderInfo
manual_ri = default_local_info { local_unit_note_len = UNIT_NOTE_4 }


demo01 :: IO ()
demo01 = printAsLilyPond global_ri phrase01

demo02 :: IO ()
demo02 = printAsLilyPond global_ri $ mapPch (`addInterval` major_second) phrase01


