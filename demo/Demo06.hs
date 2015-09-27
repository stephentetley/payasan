{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module Demo06 where

import Payasan.LilyPond.Chordmode.Notelist


-- CHORDS -- 


phrase01 :: StdChordPhrase
phrase01 = fromLilyPondWith global_ri manual_ri $ 
    [chordmode| c2 f4:1.3+.5 g:dim7 |]




global_ri :: GlobalRenderInfo
global_ri = default_global_info { global_ly_octave_mode = AbsPitch }


manual_ri :: LocalRenderInfo
manual_ri = default_local_info



demo01 :: IO ()
demo01 = printAsLilyPond global_ri phrase01

demo02 :: IO ()
demo02 = shellOutLilyPond global_ri $ outputAsLilyPond global_ri $ phrase01

demo03 :: IO ()
demo03 = writeAsMIDI "out/chords01.mid" phrase01
