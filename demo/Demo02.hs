{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module Demo02 where

import Payasan.Base.Duration
import Payasan.Base.Pitch


import Payasan.Base.Elementary.Notelist


-- MONOPHONIC

locals :: SectionInfo
locals = default_section_info

globals :: ScoreInfo
globals = default_score_info 

staff :: StaffInfo 
staff = default_staff_info


phrase01abc :: StdElemPhrase
phrase01abc = fromABCWith locals $ [abc| B4 z B B - | BB B2 z4 |]


-- Note - ElemPhrase reads (and ignores) beam group brackets.
-- Beams are re-synthesized in the output.
--
phrase01ly :: StdElemPhrase
phrase01ly = fromLilyPondWith_Relative middle_c locals $ 
    [lilypond| b'2 r8 b8 b4 ~ | b8[b] b4 r2 |]


demo01 :: IO ()
demo01 = shellOutABC default_shell_info $ 
    outputAsABC globals staff $ phrase01abc

demo02 :: IO ()
demo02 = shellOutLilyPond default_shell_info $ 
    outputAsLilyPond_Relative globals middle_c $ phrase01ly


-- Just two notes...
phrase02 :: StdElemPhrase
phrase02 = fromLilyPondWith_Relative middle_c locals $ 
    [lilypond| b'2 r8 b8 ~ b4  |]

demo03 :: IO ()
demo03 = writeAsMIDI "out/tied01.mid" phrase02



phrase10 :: StdElemPhrase
phrase10 = fromABCWith locals $ [abc| c G2 E2 C/2 | c |]

demo10 :: IO ()
demo10 = printAsABC default_score_info staff phrase10

demo11 :: IO ()
demo11 = writeAsMIDI "out/phrase1.mid" phrase10

demo12 :: IO ()
demo12 = printAsTabular default_score_info phrase10

demo13 :: IO ()
demo13 = printAsLinear default_score_info phrase10