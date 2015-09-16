{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module Demo05 where

import Payasan.Percussion.Notelist





-- DRUMS -- 

phrase01 :: StdDrumPhrase
phrase01 = fromLilyPondWith global_ri manual_ri $ 
    [drums| bda4-> bd bd bda | hh-> hh |]



global_ri :: GlobalRenderInfo
global_ri = default_global_info { global_ly_octave_mode = AbsPitch }


manual_ri :: LocalRenderInfo
manual_ri = default_local_info




demo01 :: IO ()
demo01 = printAsLilyPond global_ri phrase01


demo02 :: IO ()
demo02 = shellOutLilyPond global_ri $ outputAsLilyPond global_ri $ phrase01

demo03 :: IO ()
demo03 = writeAsMIDI "out/drums01.mid" phrase01

