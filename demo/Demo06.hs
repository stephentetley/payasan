{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module Demo06 where

import Payasan.LilyPond.Chordmode.Notelist


-- CHORDS -- 


phrase01 :: StdChordPhrase
phrase01 = fromLilyPondWith globals locals $ 
    [chordmode| c2 f4:1.3+.5 g:dim7 |]




globals :: GlobalRenderInfo
globals = default_global_info { global_ly_octave_mode = AbsPitch }


locals :: LocalRenderInfo
locals = default_local_info



demo01 :: IO ()
demo01 = printAsLilyPond globals phrase01

demo02 :: IO ()
demo02 = shellOutLilyPond globals $ outputAsLilyPond globals $ phrase01

demo03 :: IO ()
demo03 = writeAsMIDI "out/chords01.mid" phrase01


demo04 :: IO ()
demo04 = printAsTabular default_global_info phrase01

demo05 :: IO ()
demo05 = printAsLinear default_global_info phrase01

demo06 :: IO ()
demo06 = shellOutLilyPond globals $ outputAsRhythmicMarkup globals $ phrase01

