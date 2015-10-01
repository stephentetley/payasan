{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module Demo07 where

import Payasan.LilyPond.Lyricmode.Internal.Base
import Payasan.LilyPond.Lyricmode.Internal.Parser


-- LYRICS -- 

temp01 = [lyricmode| Shake4 ba8 -- by8 shake2 | Shake4 ba8 -- by8 shake.2 |  |]


{-
phrase01 :: StdLyricPhrase
phrase01 = fromLilyPondWith globals locals $ 
    [lyricmode| Shake4 ba8 -- by8 shake2 | Shake4 ba8 -- by8 shake.2 |  |]




globals :: GlobalRenderInfo
globals = default_global_info { global_ly_octave_mode = AbsPitch }


locals :: LocalRenderInfo
locals = default_local_info





demo01 :: IO ()
demo01 = printAsLilyPond global_ri phrase01

demo02 :: IO ()
demo02 = shellOutLilyPond global_ri $ outputAsLilyPond global_ri $ phrase01

demo03 :: IO ()
demo03 = writeAsMIDI "out/chords01.mid" phrase01


demo04 :: IO ()
demo04 = printAsTabular default_global_info phrase01

demo05 :: IO ()
demo05 = printAsLinear default_global_info phrase01

-}