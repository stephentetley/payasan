{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module Demo06 where

import Payasan.Chordmode.Internal.Base
import Payasan.Chordmode.Internal.Output
import Payasan.Chordmode.Internal.Parser


import Payasan.Chordmode.Notelist




-- CHORDS -- 

test01 = [chordmode| c2 f4:1.3+.5 g:dim7 |]



phrase01 :: StdChordPhrase
phrase01 = fromLilyPondWith global_ri manual_ri $ 
    [chordmode| c2 f4:1.3+.5 g:dim7 |]





global_ri :: GlobalRenderInfo
global_ri = default_global_info { global_ly_octave_mode = AbsPitch }


manual_ri :: LocalRenderInfo
manual_ri = default_local_info




demo01 :: IO ()
demo01 = printAsLilyPond global_ri phrase01

{-

demo02 :: IO ()
demo02 = shellOutLilyPond global_ri $ outputAsLilyPond global_ri $ phrase01

demo03 :: IO ()
demo03 = writeAsMIDI "out/drums01.mid" phrase01

-}