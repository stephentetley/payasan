{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module Demo05 where

import Payasan.LilyPond.Percussion.Notelist



-- DRUMS -- 

globals :: ScoreInfo
globals = default_score_info

-- voice :: VoiceInfo
-- voice = default_voice_info { voice_ly_octave_mode = AbsPitch }

locals :: SectionInfo
locals = default_section_info

-- TODO - above accents look better...
--
phrase01 :: StdDrumPhrase
phrase01 = fromLilyPondWith locals $ 
    [drums| bda4-> bd bd bda | hh-> hh |]






demo01 :: IO ()
demo01 = shellOutLilyPond default_shell_info $ outputAsLilyPond globals $ phrase01

demo02 :: IO ()
demo02 = printAsLilyPond globals phrase01

demo03 :: IO ()
demo03 = writeAsMIDI "out/drums01.mid" phrase01

demo04 :: IO ()
demo04 = printAsTabular globals phrase01

demo05 :: IO ()
demo05 = printAsLinear globals phrase01