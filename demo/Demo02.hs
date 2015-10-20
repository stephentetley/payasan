{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module Demo02 where

import Payasan.Base.Duration
import Payasan.Base.Pitch


import Payasan.Base.Monophonic.Notelist

-- MONOPHONIC

locals :: LocalContextInfo
locals = default_local_info

globals :: ScoreInfo
globals = default_score_info { global_ly_octave_mode = RelPitch middle_c }


phrase01abc :: StdMonoPhrase
phrase01abc = fromABCWith locals $ [abc| B4 z B B - | BB B2 z4 |]


-- Note - MonoPhrase cannot read beam group brackets
-- TODO - Maybe it should ignore them?
--
phrase01ly :: StdMonoPhrase
phrase01ly = fromLilyPondWith globals locals $ 
    [lilypond| b'2 r8 b8 b4 ~ | b8 b b4 r2 |]


demo01 :: IO ()
demo01 = shellOutABC globals $ outputAsABC globals $ phrase01abc

demo02 :: IO ()
demo02 = shellOutLilyPond globals $ outputAsLilyPond globals $ phrase01ly


-- Just two notes...
phrase02 :: StdMonoPhrase
phrase02 = fromLilyPondWith globals locals $ 
    [lilypond| b'2 r8 b8 ~ b4  |]

demo03 :: IO ()
demo03 = writeAsMIDI "out/tied01.mid" phrase02



phrase10 :: StdMonoPhrase
phrase10 = fromABCWith locals $ [abc| c G2 E2 C/2 | c |]

demo10 :: IO ()
demo10 = printAsABC default_score_info phrase10

demo11 :: IO ()
demo11 = writeAsMIDI "out/phrase1.mid" phrase10

demo12 :: IO ()
demo12 = printAsTabular default_score_info phrase10

demo13 :: IO ()
demo13 = printAsLinear default_score_info phrase10