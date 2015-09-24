{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module TestScales1 where

import Payasan.Base.Monophonic.Notelist

import Payasan.Base.Duration
import Payasan.Base.Pitch
import Payasan.Base.Names.Interval
import Payasan.Base.Names.Key
import Payasan.Base.Names.Pitch

import Payasan.Base.ScaleDegree                 -- TEMP
import Payasan.Base.Internal.ABC.Spelling       -- TEMP
import Payasan.Base.Monophonic.Internal.Plain   -- TEMP


a_major_ly :: StdMonoPhrase
a_major_ly = fromLilyPondWith globals locals $ 
    [lilypond| a4 b cis d | e fis gis a |]


a_major_abc :: StdMonoPhrase
a_major_abc = fromABCWith locals $ 
    [abc| A, B, C D | E F G A |]



a_major_plain :: StdMonoPhrase
a_major_plain = fromNoteList locals $
    map (\p -> note p dQuarter) $ [ a_3, b_3, cs_4, d_4, e_4, fs_4, gs_4, a_4  ]

globals :: GlobalRenderInfo
globals = default_global_info { global_ly_octave_mode = RelPitch middle_c  }


locals :: LocalRenderInfo
locals = default_local_info { local_unit_note_len = UNIT_NOTE_4
                            , local_key = a_major  }



demo01 :: IO ()
demo01 = 
    do { printAsTabular globals a_major_ly
       ; shellOutLilyPond globals $ outputAsLilyPond globals $ a_major_ly
       ; shellOutABC globals $ outputAsABC globals $ a_major_ly
       }


demo02 :: IO ()
demo02 = 
    do { printAsTabular globals a_major_abc
       ; shellOutLilyPond globals $ outputAsLilyPond globals $ a_major_abc
       ; shellOutABC globals $ outputAsABC globals $ a_major_abc
       }

demo03 :: IO ()
demo03 = 
    do { printAsTabular globals a_major_plain
       ; shellOutLilyPond globals $ outputAsLilyPond globals $ a_major_plain
       ; shellOutABC globals $ outputAsABC globals $ a_major_plain
       }


