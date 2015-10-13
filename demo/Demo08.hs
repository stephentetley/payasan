{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module Demo08 where

import Payasan.LilyPond.FretDiagram.Internal.Base
import Payasan.LilyPond.FretDiagram.Internal.Interpret
import Payasan.LilyPond.FretDiagram.Internal.Parser
import Payasan.LilyPond.FretDiagram.Top

import Payasan.Base.Monophonic.Notelist
import Payasan.Base.Monophonic.Internal.Plain

import Payasan.Base.Duration
import Payasan.Base.Pitch
import Payasan.Base.ScaleDegree
import Payasan.Base.Names.DiatonicInterval
import Payasan.Base.Names.Key
import Payasan.Base.Names.Pitch

import Payasan.Base.Internal.Base 


import Text.PrettyPrint.HughesPJClass           -- package: pretty


-- FRET DIAGRAMS -- 

test01 = pPrint $ [fret_diagram| 6-x;5-x;4-o;3-2;2-3;1-2;  |]
test02 = pPrint $ [fret_diagram| c:6-1-1;6-1;5-1;4-1;3-2;2-3;1-1;  |]


demo01 :: IO ()
demo01 = shellOutLilyPond globals $ outputAsLilyPond globals $ 
    fromNoteList locals $ map (\p -> note p d_quarter) $ standard_tuning

demo02 :: IO ()
demo02 = shellOutLilyPond globals $ outputAsLilyPond globals $ 
    fromNoteList locals $ map (\p -> note p d_quarter) $ pitches
  where
    pitches :: [Pitch]
    pitches = interpretFretDiagram c_major standard_tuning $ 
                [fret_diagram| 6-x;5-x;4-o;3-2;2-3;1-2;  |]

-- This shows error in transposeWithDiatonicInterval ** NOW FIXED
--
test03 :: IO ()
test03 = shellOutLilyPond globals $ outputAsLilyPond globals $ 
    fromNoteList locals $ map (\p -> note p d_quarter) $ pitches
  where
    pitches = let fn = transposeWithDiatonicInterval c_major 
              in [ fn simple_unison d_4
                 , fn simple_third  g_4
                 , fn simple_fourth b_4
                 , fn simple_third  e_5 
                 ]
{-

test04a, test04b, test04c :: Pitch
test04a = transposeWithDiatonicInterval c_major simple_unison e_5
test04b = transposeWithDiatonicInterval c_major simple_unison a_4
test04c = transposeWithDiatonicInterval c_major simple_unison g_4
    

test05a = fromPitch c_major e_5
test05b = fromPitch c_major a_4 
test05c = fromPitch c_major g_4 
-}


{-

phrase01 :: StdLyricPhrase
phrase01 = fromLilyPondWith globals locals $ 
    [lyricmode| Shake4 ba8 -- by8 shake2 | Shake4 ba8 -- by8 shake.2 |  |]

-}


globals :: ScoreInfo
globals = default_score_info { global_ly_octave_mode = AbsPitch }


locals :: LocalContextInfo
locals = default_local_info



{-

demo01 :: IO ()
demo01 = printAsLilyPond globals phrase01



demo02 :: IO ()
demo02 = shellOutLilyPond global_ri $ outputAsLilyPond global_ri $ 
   fromNoteList $ map (\p -> note p d_quarter) $ standard_tuning


demo03 :: IO ()
demo03 = writeAsMIDI "out/chords01.mid" phrase01


demo04 :: IO ()
demo04 = printAsTabular default_global_info phrase01

demo05 :: IO ()
demo05 = printAsLinear default_global_info phrase01

-}