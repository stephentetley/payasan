{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module Demo01 where

import Payasan.Base.Internal.ABC.Output (abcOutput) -- temp
import Payasan.Base.Notelist




phrase01 :: StdPhrase
phrase01 = fromABC $ [abc| [cg] G2 E2 C/2 | c |]

locals :: LocalContextInfo
locals = default_local_info { local_unit_note_len = UNIT_NOTE_4 }


demo01 :: IO ()
demo01 = printAsABC default_score_info default_staff_info phrase01

demo01a :: IO ()
demo01a = writeAsMIDI "out/phrase1.mid" phrase01

-- Be care to judge on beaming - manual may be non-standard
phrase02 :: StdPhrase
phrase02 = fromABCWith locals $ 
    [abc| c2 {d}c {c2d2}c|{dcd}c {ede}d {fef}e f| c/{gfef}d/e/f/ f/e/{gfedc}d/c/|c G E {cBAGFED}C| |]

demo02 :: IO ()
demo02 = printAsABC default_score_info default_staff_info phrase02

phrase03 :: ABCPhrase
phrase03 = [abc|
    (3cde e2 | (6cegczg (3czg | (3:2:2G4c2 | (3:2:4G2A2Bc | (3:2:6(3GGGA2Bc |]

demo03:: IO ()
demo03 = putStrLn $ ppRender $ abcOutput default_score_info default_staff_info phrase03


testPh :: StdPhrase
testPh = fromABCWith locals $ 
    [abc| c c//c//c//c//  c/4c/4c/4c/4 c |]

test01 :: IO ()
test01 = printAsABC default_score_info default_staff_info testPh


testMiddleC :: StdPhrase
testMiddleC = fromABCWith locals $ 
    [abc| C |]
