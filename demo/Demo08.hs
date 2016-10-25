{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module Demo08 where

import Payasan.LilyPond.FretDiagram.Notelist

import Payasan.LilyPond.FretDiagram.Internal.Interpret  -- TEMP

import qualified Payasan.Score.Elementary.Notelist as MONO
import qualified Payasan.Score.Elementary.Internal.Plain as MONO

import Payasan.Base.Duration
import Payasan.Base.Pitch
import Payasan.Base.Diatonic
import Payasan.Base.Names.DiatonicInterval
import Payasan.Base.Names.Key
import Payasan.Base.Names.Pitch



import Text.PrettyPrint.HughesPJClass           -- package: pretty

-- Simplistic printing of fret diagrams is diagram+chord on standard staff

-- FRET DIAGRAMS -- 


globals :: ScoreInfo
globals = default_score_info

locals :: SectionInfo
locals = default_section_info



dia01 :: FretDiagram
dia01 = pushName "chordX" $ [fret_diagram| c:6-1-1;6-1;5-1;4-1;3-2;2-3;1-1;  |]

dia02 :: FretDiagram
dia02 = pushName "chordY" $ [fret_diagram| 6-x;5-x;4-o;3-2;2-3;1-2;  |]


demo01 :: IO ()
demo01 = shellOutLilyPond default_shell_info $ 
    outputAsLilyPond globals [dia01,dia02] $ 
    fromNoteList locals standard_tuning [ [ chord dia01 d_whole ]
                                        , [ chord dia02 d_whole ] ]


temp01 :: IO ()
temp01 = shellOutLilyPond default_shell_info $ 
    MONO.outputAsLilyPond_Relative globals middle_c $ 
    MONO.fromNoteList locals $ map (\p -> MONO.note p d_quarter) $ standard_tuning

temp02 :: IO ()
temp02 = shellOutLilyPond default_shell_info $ 
    MONO.outputAsLilyPond_Relative globals middle_c $ 
    MONO.fromNoteList locals $ map (\p -> MONO.note p d_quarter) $ pitches
  where
    pitches :: [Pitch]
    pitches = interpretFretDiagram c_major standard_tuning $ 
                [fret_diagram| 6-x;5-x;4-o;3-2;2-3;1-2;  |]


test01 = pPrint $ [fret_diagram| 6-x;5-x;4-o;3-2;2-3;1-2;  |]


-- This was showing an error in transposeWithDiatonicInterval ** NOW FIXED
--
test02 :: IO ()
test02 = MONO.shellOutLilyPond default_shell_info $ 
    MONO.outputAsLilyPond_Relative globals middle_c $ 
    MONO.fromNoteList locals $ map (\p -> MONO.note p d_quarter) $ pitches
  where
    pitches = let fn = transposeDiatonically c_major 
              in [ fn simple_unison d_4
                 , fn simple_third  g_4
                 , fn simple_fourth b_4
                 , fn simple_third  e_5 
                 ]



