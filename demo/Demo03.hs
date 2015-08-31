{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module Demo03 where

import Payasan.Base.Duration
import Payasan.Base.Pitch


import Payasan.Base.Internal.LilyPondParser
import Payasan.Base.Internal.LilyPondSyntax
import Payasan.Base.Internal.LilyPondInTrans


import Text.Parsec       -- TEMP


test01 = parseTest pitch "ces'"

test02 = parseTest noteLength "4.."
test03 = parseTest note "dis'8"
test04 = parseTest tupletSpec "\\tuplet 3/4"


phrase01 :: LyPhrase
phrase01 = [lilypond| c d e fis | c |]

{-

phrase01 :: StdMonoPhrase
phrase01 = fromLilyPondWith manual_ri $ [lilypond| c G2 E2 C/2 | c |]



manual_ri :: RenderInfo
manual_ri = default_render_info { render_unit_note_len = UNIT_NOTE_4 }


demo01 :: IO ()
demo01 = printAsABC phrase01

demo01a :: IO ()
demo01a = writeAsMIDI "out/phrase1.mid" phrase01

-}