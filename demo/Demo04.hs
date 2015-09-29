{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module Demo04 where

import Payasan.Base.Monophonic.Notelist

-- import Payasan.Base.Duration
import Payasan.Base.Pitch
import Payasan.Base.Names.DiatonicInterval
import Payasan.Base.Names.Interval
import Payasan.Base.Names.Key
import Payasan.Base.Names.Pitch

import Payasan.Base.Monophonic.Internal.Metrics
-- import Payasan.Base.Internal.Linear.Common
-- import Payasan.Base.Internal.Linear.Utils

-- TRANSFORMATIONS -- 

phrase01 :: StdMonoPhrase
phrase01 = fromLilyPondWith global_ri manual_ri $ 
    [lilypond| d2 a' | f d |]



global_ri :: GlobalRenderInfo
global_ri = default_global_info { global_ly_octave_mode = RelPitch middle_c  }


manual_ri :: LocalRenderInfo
manual_ri = default_local_info { local_unit_note_len = UNIT_NOTE_4
                               , local_key = d_minor  }


demo01 :: IO ()
demo01 = 
    do { printAsLilyPond global_ri phrase01
       ; shellOutLilyPond global_ri $ outputAsLilyPond global_ri $ phrase01
       ; putStrLn ""
       ; printAsABC global_ri phrase01
       ; shellOutABC global_ri $ outputAsABC global_ri $ phrase01
       }

demo02 :: IO ()
demo02 = printAsLilyPond global_ri $ mapPitch (.+^ major_second) phrase01

demo03 :: IO ()
demo03 = printAsLilyPond global_ri $ augment phrase01

demo04 :: IO ()
demo04 = shellOutABC global_ri $ outputAsABC global_ri $ augment phrase01

demo05 :: IO ()
demo05 = shellOutLilyPond global_ri $ outputAsLilyPond global_ri $ 
    augment phrase01

demo06 :: IO ()
demo06 = shellOutLilyPond global_ri $ outputAsLilyPond global_ri $ 
    retrograde phrase01

demo07 :: IO ()
demo07 = shellOutLilyPond global_ri $ outputAsLilyPond global_ri $ 
    invertChromatic phrase01

demo08 :: IO ()
demo08 = shellOutLilyPond global_ri $ outputAsLilyPond global_ri $ 
    transposeDiatonic simple_second phrase01

demo09 :: IO ()
demo09 = shellOutLilyPond global_ri $ outputAsLilyPond global_ri $ 
    invertDiatonic phrase01

-- TODO - need linear output...
demo10 = semitoneInterval phrase01