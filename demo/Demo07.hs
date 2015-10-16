{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module Demo07 where

import Payasan.LilyPond.Lyricmode.Notelist

import Payasan.LilyPond.Lyricmode.Internal.Interpret    -- TEMP


-- LYRICS -- 

-- NOTE - do not beam lyrics...


phrase01 :: StdLyricPhrase
phrase01 = fromLilyPondWith globals locals $ 
    [lyricmode| Shake4 ba8 -- by8 shake2 | Shake4 ba8 -- by8 shake.2 |  |]




globals :: ScoreInfo
globals = default_score_info { global_ly_octave_mode = AbsPitch }


locals :: LocalContextInfo
locals = default_local_info



demo01  :: IO ()
demo01 = shellOutLilyPond globals $ outputAsLilyPond globals $ phrase01

demo02 :: IO ()
demo02 = printAsLilyPond globals phrase01



