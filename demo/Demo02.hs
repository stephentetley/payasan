{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module Demo02 where

-- import Payasan.Base.Notelist
import Payasan.Base.Duration
import Payasan.Base.Pitch

import Payasan.Base.Internal.MidiOutput
import Payasan.Base.Internal.MidiOutTrans
import Payasan.Base.Internal.MidiSyntax

import Payasan.Base.Monophonic.Internal.MonoSyntax
import Payasan.Base.Monophonic.Internal.ABCParser
import Payasan.Base.Monophonic.Internal.ABCInTrans
import Payasan.Base.Monophonic.Internal.MonoOutTrans
import Payasan.Base.Monophonic.Notelist

phrase01 :: StdMonoPhrase
phrase01 = fromABCWith manual_ri $ [abc| c G2 E2 C/2 | c |]



manual_ri :: RenderInfo
manual_ri = default_render_info { render_unit_note_len = UNIT_NOTE_4 }


demo01 :: IO ()
demo01 = printAsABC phrase01

demo01a :: IO ()
demo01a = writeAsMIDI "out/phrase1.mid" phrase01

