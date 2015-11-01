{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module Demo06 where

import Payasan.LilyPond.Chordmode.Notelist


-- CHORDS -- 


globals :: ScoreInfo
globals = default_score_info


locals :: LocalContextInfo
locals = default_local_info


phrase01 :: StdChordPhrase
phrase01 = fromLilyPondWith locals $ 
    [chordmode| c2 f4:1.3+.5 g:dim7 |]






demo01 :: IO ()
demo01 = shellOutLilyPond default_shell_info $ outputAsLilyPond globals $ phrase01

demo02 :: IO ()
demo02 = printAsLilyPond globals phrase01

demo03 :: IO ()
demo03 = writeAsMIDI "out/chords01.mid" phrase01


demo04 :: IO ()
demo04 = printAsTabular globals phrase01

demo05 :: IO ()
demo05 = printAsLinear globals phrase01

demo06 :: IO ()
demo06 = shellOutLilyPond default_shell_info $ outputAsRhythmicMarkup globals $ phrase01

