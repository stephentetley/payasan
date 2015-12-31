{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module Demo04 where

import Payasan.Base.Elementary.Notelist
import Payasan.Base.Elementary.Internal.TakeDrop

-- import Payasan.Base.Duration
import Payasan.Base.Pitch
import Payasan.Base.Names.DiatonicInterval
import Payasan.Base.Names.Interval
import Payasan.Base.Names.Key
import Payasan.Base.Names.Pitch

import Payasan.Base.Elementary.Internal.Metrics
import Payasan.Base.ScaleDegree


-- Note to me - debugging with a rhythm staff and markup would be good.

-- TRANSFORMATIONS -- 


globals :: ScoreInfo
globals = default_score_info

-- voice :: VoiceInfo
-- voice = default_voice_info { voice_ly_octave_mode = RelPitch middle_c  }

staff :: StaffInfo
staff = default_staff_info

locals :: SectionInfo
locals = default_section_info { section_unit_note_len = UNIT_NOTE_4
                              , section_key = d_minor  }


phrase01 :: StdElemPhrase
phrase01 = fromLilyPondWith_Relative middle_c locals $ 
    [lilypond| d2 a' | f d |]



demo01 :: IO ()
demo01 = 
    do { printAsLilyPond_Relative globals middle_c phrase01
       ; shellOutLilyPond default_shell_info $ outputAsLilyPond_Relative globals middle_c $ phrase01
       ; putStrLn ""
       ; printAsABC globals staff phrase01
       ; shellOutABC default_shell_info $ outputAsABC globals staff $ phrase01
       ; printAsTabular globals phrase01
       }

demo02 :: IO ()
demo02 = shellOutLilyPond default_shell_info $ 
    outputAsLilyPond_Relative globals middle_c $ mapPitch (.+^ major_second) phrase01

demo03 :: IO ()
demo03 = printAsLilyPond_Relative globals middle_c $ augment phrase01

demo04 :: IO ()
demo04 = shellOutABC default_shell_info $ 
    outputAsABC globals staff $ augment phrase01

demo05 :: IO ()
demo05 = shellOutLilyPond default_shell_info $ 
    outputAsLilyPond_Relative globals middle_c $ augment phrase01

demo06 :: IO ()
demo06 = shellOutLilyPond default_shell_info $ 
    outputAsLilyPond_Relative globals middle_c $ retrograde phrase01

demo07 :: IO ()
demo07 = shellOutLilyPond default_shell_info $ 
    outputAsLilyPond_Relative globals middle_c $ invertChromatic phrase01

demo08 :: IO ()
demo08 = shellOutLilyPond default_shell_info $ 
    outputAsLilyPond_Relative globals middle_c $ transposeDiatonic simple_second phrase01

demo09 :: IO ()
demo09 = shellOutLilyPond default_shell_info $ 
    outputAsLilyPond_Relative globals middle_c $ invertDiatonic phrase01

demo10 :: IO ()
demo10 = do 
    print $ pitchHisto $ phrase01
    print $ pitchNameHisto $ phrase01
    print $ octaveHisto $ phrase01
    print $ grossContour $ phrase01

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