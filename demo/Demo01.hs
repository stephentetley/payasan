{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module Demo01 where

import qualified Payasan.PSC.ABC.Compile as ABC

import Payasan.PSC.Repr.External.ABCInTrans
import Payasan.PSC.Repr.External.ABCParser
import Payasan.PSC.Repr.External.Syntax

import Payasan.PSC.Base.SyntaxCommon


abc_compiler ::ABC.Compiler
abc_compiler = ABC.makeCompiler def
  where 
    def = ABC.emptyDef { ABC.pathto_working_dir     = "PAYASAN_TEMP_DIR"
                       , ABC.outfile_name           = "abc_output.abc"
                       }

compileABC :: StdPart -> IO ()
compileABC = ABC.compile abc_compiler


s01 :: ABCSectionQuote
s01 = [abc| [cg] G2 E2 C/2 | c |]

locals :: SectionInfo
locals = default_section_info { section_unit_note_len = UNIT_NOTE_4 }

section01 :: StdSection
section01 = unquoteABC "Phrase1" locals s01




demo01 :: IO ()
demo01 = compileABC (Part { part_sections = [section01] })


{-
demo01a :: IO ()
demo01a = writeAsMIDI "out/phrase1.mid" phrase01

-- Be care to judge on beaming - manual may be non-standard
phrase02 :: StdPart
phrase02 = fromABCWith locals $ 
    [abc| c2 {d}c {c2d2}c|{dcd}c {ede}d {fef}e f| c/{gfef}d/e/f/ f/e/{gfedc}d/c/|c G E {cBAGFED}C| |]

demo02 :: IO ()
demo02 = printAsABC default_score_info default_staff_info phrase02

phrase03 :: StdPart
phrase03 = fromABCWith locals $ 
    [abc| (3cde e2 | (6cegczg (3czg | (3:2:2G4c2 | (3:2:4G2A2Bc | (3:2:6(3GGGA2Bc |]

demo03:: IO ()
demo03 = printAsABC default_score_info default_staff_info phrase03


testPh :: StdPart
testPh = fromABCWith locals $ 
    [abc| c c//c//c//c//  c/4c/4c/4c/4 c |]

test01 :: IO ()
test01 = printAsABC default_score_info default_staff_info testPh


testMiddleC :: StdPart
testMiddleC = fromABCWith locals $ 
    [abc| C |]

-}