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
import Payasan.Base.ScaleDegree


-- Note to me - debugging with a rhythm staff and markup would be good.

-- TRANSFORMATIONS -- 

phrase01 :: StdMonoPhrase
phrase01 = fromLilyPondWith globals manuals $ 
    [lilypond| d2 a' | f d |]



globals :: GlobalRenderInfo
globals = default_global_info { global_ly_octave_mode = RelPitch middle_c  }


manuals :: LocalRenderInfo
manuals = default_local_info { local_unit_note_len = UNIT_NOTE_4
                             , local_key = d_minor  }


demo01 :: IO ()
demo01 = 
    do { printAsLilyPond globals phrase01
       ; shellOutLilyPond globals $ outputAsLilyPond globals $ phrase01
       ; putStrLn ""
       ; printAsABC globals phrase01
       ; shellOutABC globals $ outputAsABC globals $ phrase01
       ; printAsTabular globals phrase01
       }

demo02 :: IO ()
demo02 = shellOutLilyPond globals $ outputAsLilyPond globals $ mapPitch (.+^ major_second) phrase01

demo03 :: IO ()
demo03 = printAsLilyPond globals $ augment phrase01

demo04 :: IO ()
demo04 = shellOutABC globals $ outputAsABC globals $ augment phrase01

demo05 :: IO ()
demo05 = shellOutLilyPond globals $ outputAsLilyPond globals $ 
    augment phrase01

demo06 :: IO ()
demo06 = shellOutLilyPond globals $ outputAsLilyPond globals $ 
    retrograde phrase01

demo07 :: IO ()
demo07 = shellOutLilyPond globals $ outputAsLilyPond globals $ 
    invertChromatic phrase01

demo08 :: IO ()
demo08 = shellOutLilyPond globals $ outputAsLilyPond globals $ 
    transposeDiatonic simple_second phrase01

demo09 :: IO ()
demo09 = shellOutLilyPond globals $ outputAsLilyPond globals $ 
    invertDiatonic phrase01

{-

test09 = printAsTabular globals $ diatonicsFromTop $ mapPitch (toChromaticPitch d_minor) $ phrase01

test09b = fromChromaticPitch d_minor $ 
    mkC $ addDiatonicInterval (DiatonicPitch TONIC 0) (DiatonicInterval FOURTH 0) 

mkC dp = ChromaticPitch dp 0


test09c = diatonicIntervalBetween (diatonic_base $ toChromaticPitch d_minor d_4) 
                                  (diatonic_base $ toChromaticPitch d_minor a_4)

test09d = fmap (fromChromaticPitch d_minor . mkC) $ lowestStep $ mapPitch (toChromaticPitch d_minor) $ phrase01

-- currently gets ove wrong...
test09e = fromChromaticPitch d_minor $ mkC $ (DiatonicPitch TONIC 0) `addDiatonicInterval` simple_fifth



test09f = fromChromaticPitch d_minor $ mkC $ (DiatonicPitch TONIC 0) `addDiatonicInterval` simple_unison

test09g = fromChromaticPitch d_minor $ mkC $ (DiatonicPitch TONIC 0) 

-}