{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module TestScales1 where

import Payasan.Base.Elementary.Notelist

import Payasan.Base.Duration
import Payasan.Base.Pitch
import Payasan.Base.Names.Interval
import Payasan.Base.Names.Key
import Payasan.Base.Names.Pitch

import Payasan.Base.ScaleDegree                 -- TEMP
import Payasan.Base.Elementary.Internal.Plain   -- TEMP

-- a_major_temp :: Phrase LyPitch LyNoteLength
a_major_temp = [lilypond| a4 b cis d | e fis gis a |]

a_major_ly :: StdMonoPhrase
a_major_ly = fromLilyPondWith_Relative middle_c locals $ 
    [lilypond| a4 b cis d | e fis gis a |]


a_major_abc :: StdMonoPhrase
a_major_abc = fromABCWith locals $ 
    [abc| A, B, C D | E F G A |]



a_major_plain :: StdMonoPhrase
a_major_plain = fromNoteList locals $
    map (\p -> note p d_quarter) $ [ a_3, b_3, cs_4, d_4, e_4, fs_4, gs_4, a_4  ]

globals :: ScoreInfo
globals = default_score_info 

-- voice :: VoiceInfo 
-- voice = default_voice_info { voice_ly_octave_mode = RelPitch middle_c  }

staff :: StaffInfo
staff = default_staff_info

locals :: SectionInfo
locals = default_section_info { section_unit_note_len = UNIT_NOTE_4
                              , section_key = a_major  }



demo01 :: IO ()
demo01 = 
    do { printAsTabular globals a_major_ly
       ; shellOutLilyPond default_shell_info $ outputAsLilyPond_Relative globals middle_c $ a_major_ly
       ; shellOutABC default_shell_info $ outputAsABC globals staff $ a_major_ly
       }


demo02 :: IO ()
demo02 = 
    do { printAsTabular globals a_major_abc
       ; shellOutLilyPond default_shell_info $ outputAsLilyPond_Relative globals middle_c $ a_major_abc
       ; shellOutABC default_shell_info $ outputAsABC globals staff $ a_major_abc
       }

demo03 :: IO ()
demo03 = 
    do { printAsTabular globals a_major_plain
       ; shellOutLilyPond default_shell_info $ outputAsLilyPond_Relative globals middle_c $ a_major_plain
       ; shellOutABC default_shell_info $ outputAsABC globals staff $ a_major_plain
       }


