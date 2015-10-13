{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module Demo05 where

import Payasan.LilyPond.Percussion.Notelist



-- DRUMS -- 

phrase01 :: StdDrumPhrase
phrase01 = fromLilyPondWith globals locals $ 
    [drums| bda4-> bd bd bda | hh-> hh |]



globals :: ScoreInfo
globals = default_score_info { global_ly_octave_mode = AbsPitch }


locals :: LocalContextInfo
locals = default_local_info




demo01 :: IO ()
demo01 = printAsLilyPond globals phrase01


demo02 :: IO ()
demo02 = shellOutLilyPond globals $ outputAsLilyPond globals $ phrase01

demo03 :: IO ()
demo03 = writeAsMIDI "out/drums01.mid" phrase01

demo04 :: IO ()
demo04 = printAsTabular globals phrase01

demo05 :: IO ()
demo05 = printAsLinear globals phrase01