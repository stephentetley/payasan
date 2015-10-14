{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module Demo03 where


-- import Payasan.Base.Duration
import Payasan.Base.Notelist
import Payasan.Base.Pitch

import Payasan.Base.Internal.LilyPond.Syntax (fromPitchRel, LyPitch)

import Payasan.Base.Names.Pitch

import Payasan.Base.Internal.Output.Common
import Payasan.Base.Internal.Output.Tabular.OutputBeam




phrase01 :: StdPhrase 
phrase01 = fromLilyPondWith globals locals $ 
    [lilypond| c4 d e fis | c' |]


debug01 :: IO StdPhrase
debug01 = fromLilyPondWithIO globals locals $ 
    [lilypond| c4 d e fis | c' |]



globals :: ScoreInfo
globals = 
    default_score_info { global_ly_octave_mode = RelPitch middle_c }


locals :: LocalContextInfo
locals = default_local_info { local_unit_note_len = UNIT_NOTE_4 }


demo01 :: IO ()
demo01 = printAsLilyPond globals phrase01

demo01a :: IO ()
demo01a = printAsABC globals phrase01


test01,test02, test03 :: Int
test01 = octaveCount $ Interval 9 13
test02 = octaveCount $ Interval 1 0
test03 = octaveCount $ Interval 8 12

test04 :: Interval
test04 = simpleIntervalOf $ Interval 8 12

test05 :: LyPitch
test05 = fromPitchRel c_5 fs_4

test06 = beamTabular std_ly_output $ [lilypond| c4 d e fis | c' |]


test07 :: LyPitch
test07 = fromPitchRel e_4 ff_4
