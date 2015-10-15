{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module Demo08 where

import Payasan.LilyPond.FretDiagram.Internal.Base
import Payasan.LilyPond.FretDiagram.Internal.Interpret
import Payasan.LilyPond.FretDiagram.Internal.Parser
import Payasan.LilyPond.FretDiagram.Top

import qualified Payasan.Base.Monophonic.Notelist as MONO
import qualified Payasan.Base.Monophonic.Internal.Plain as MONO

import Payasan.Base.Duration
import Payasan.Base.Pitch
import Payasan.Base.ScaleDegree
import Payasan.Base.Names.DiatonicInterval
import Payasan.Base.Names.Key
import Payasan.Base.Names.Pitch

import Payasan.Base.Internal.Base 


import Text.PrettyPrint.HughesPJClass           -- package: pretty

-- Simplistic printing of fret diagrams is diagram+chord on standard staff

-- FRET DIAGRAMS -- 


globals :: ScoreInfo
globals = default_score_info { global_ly_octave_mode = AbsPitch }


locals :: LocalContextInfo
locals = default_local_info


test01 = pPrint $ [fret_diagram| 6-x;5-x;4-o;3-2;2-3;1-2;  |]
test02 = asDefinition $ pushName "mychord" $ [fret_diagram| c:6-1-1;6-1;5-1;4-1;3-2;2-3;1-1;  |]
test03 = asDefinition $ pushName "mychord" $ [fret_diagram| c:6-1-1;6-1;5-1;4-1;3-2;2-3;1-1;  |]

dia01 = pushName "mychord" $ [fret_diagram| c:6-1-1;6-1;5-1;4-1;3-2;2-3;1-1;  |]


demo01 :: IO ()
demo01 = shellOutLilyPond globals $ MONO.outputAsLilyPond globals $ 
    MONO.fromNoteList locals $ map (\p -> MONO.note p d_quarter) $ standard_tuning

demo02 :: IO ()
demo02 = shellOutLilyPond globals $ MONO.outputAsLilyPond globals $ 
    MONO.fromNoteList locals $ map (\p -> MONO.note p d_quarter) $ pitches
  where
    pitches :: [Pitch]
    pitches = interpretFretDiagram c_major standard_tuning $ 
                [fret_diagram| 6-x;5-x;4-o;3-2;2-3;1-2;  |]


demo03 = shellOutLilyPond globals $ outputAsLilyPond globals $ 
    fromNoteList locals standard_tuning [[ chord dia01 d_whole ]]

-- This shows error in transposeWithDiatonicInterval ** NOW FIXED
--
test04 :: IO ()
test04 = MONO.shellOutLilyPond globals $ MONO.outputAsLilyPond globals $ 
    MONO.fromNoteList locals $ map (\p -> MONO.note p d_quarter) $ pitches
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




