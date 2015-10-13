{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module Demo02 where

import Payasan.Base.Duration
import Payasan.Base.Pitch


import Payasan.Base.Monophonic.Notelist



phrase01 :: StdMonoPhrase
phrase01 = fromABCWith manual_ri $ [abc| c G2 E2 C/2 | c |]



manual_ri :: LocalContextInfo
manual_ri = default_local_info { local_unit_note_len = UNIT_NOTE_4 }


demo01 :: IO ()
demo01 = printAsABC default_score_info phrase01

demo01a :: IO ()
demo01a = writeAsMIDI "out/phrase1.mid" phrase01

demo02 :: IO ()
demo02 = printAsTabular default_score_info phrase01

demo03 :: IO ()
demo03 = printAsLinear default_score_info phrase01