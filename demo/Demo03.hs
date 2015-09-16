{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module Demo03 where


import Payasan.Base.Duration
import Payasan.Base.Notelist
import Payasan.Base.Pitch

import Payasan.Base.Internal.LilyPond.OutTrans
import Payasan.Base.Internal.LilyPond.Output
import Payasan.Base.Internal.LilyPond.Utils

import Payasan.Base.Names.Interval
import Payasan.Base.Names.Pitch

import Payasan.Base.Internal.Tabular.Common
import Payasan.Base.Internal.Tabular.OutputBeam
import Payasan.Base.Internal.Tabular.OutputMain


import Text.PrettyPrint.HughesPJClass        -- package: pretty


phrase01 :: StdPhrase 
phrase01 = fromLilyPondWith global_ri manual_ri $ 
    [lilypond| c4 d e fis | c' |]


debug01 :: IO StdPhrase
debug01 = fromLilyPondWithIO global_ri manual_ri $ 
    [lilypond| c4 d e fis | c' |]



global_ri :: GlobalRenderInfo
global_ri = 
    default_global_info { global_ly_octave_mode = RelPitch middle_c }


manual_ri :: LocalRenderInfo
manual_ri = default_local_info { local_unit_note_len = UNIT_NOTE_4 }


demo01 :: IO ()
demo01 = printAsLilyPond global_ri phrase01

demo01a :: IO ()
demo01a = printAsABC global_ri phrase01


test01,test02, test03 :: Int
test01 = octaveCount $ Interval 9 13
test02 = octaveCount $ Interval 1 0
test03 = octaveCount $ Interval 8 12
test04 = simpleIntervalOf $ Interval 8 12


test05 = fromPitchRel c_5 fs_4

test06 = beamTabular std_ly_output $ [lilypond| c4 d e fis | c' |]


test07 = fromPitchRel e_4 ff_4
