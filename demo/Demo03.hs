{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module Demo03 where


import Payasan.Base.Duration
import Payasan.Base.Notelist
import Payasan.Base.Pitch


import Payasan.Base.Internal.LilyPondParser
import Payasan.Base.Internal.LilyPondSyntax
import Payasan.Base.Internal.LilyPondInTrans


import Text.Parsec       -- TEMP


test01 = parseTest pitch "ces'"

test02 = parseTest noteLength "4.."
test03 = parseTest note "dis'8"
test04 = parseTest tupletSpec "\\tuplet 3/4"


phrase01 :: StdPhrase
phrase01 = fromLilyPondWith global_ri manual_ri $ 
    [lilypond| c d e fis | c |]


global_ri :: GlobalRenderInfo
global_ri = GlobalRenderInfo
    { global_pitch_directive = RelPitch middle_c
    }


manual_ri :: LocalRenderInfo
manual_ri = default_local_info { local_unit_note_len = UNIT_NOTE_4 }


demo01 :: IO ()
demo01 = printAsABC phrase01

