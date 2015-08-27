{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module Demo01 where

import Payasan.Base.Internal.ABCOutput (abcOutput) -- temp
import Payasan.Base.Notelist

import Payasan.Base.Internal.MidiOutput


phrase01 :: StdPhrase
phrase01 = fromABC $ [abcPhrase| [cg] G2 E2 C/2 | c |]

manual_ri :: RenderInfo
manual_ri = default_render_info { render_unit_note_len      = UNIT_NOTE_4 }


demo01 :: IO ()
demo01 = printAsABC phrase01

-- Be care to judge on beaming - manual may be non-standard
phrase02 :: StdPhrase
phrase02 = fromABCWith manual_ri $ 
    [abcPhrase| c2 {d}c {c2d2}c|{dcd}c {ede}d {fef}e f| c/{gfef}d/e/f/ f/e/{gfedc}d/c/|c G E {cBAGFED}C| |]

demo02 :: IO ()
demo02 = printAsABC phrase02

phrase03 :: ABCPhrase
phrase03 = [abcPhrase|
    (3cde e2 | (6cegczg (3czg | (3:2:2G4c2 | (3:2:4G2A2Bc | (3:2:6(3GGGA2Bc |]

demo03:: IO ()
demo03 = putStrLn $ ppRender $ abcOutput phrase03


-- Beaming wrong - todo
testPh :: StdPhrase
testPh = fromABCWith manual_ri $ 
    [abcPhrase| c c//c//c//c//  c/4c/4c/4c/4 c |]

test01 :: IO ()
test01 = printAsABC testPh
