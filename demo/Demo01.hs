{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module Demo01 where

import qualified Payasan.PSC.ABC.Compile            as ABC
import Payasan.PSC.ABC.ExternalUnquote

import qualified Payasan.PSC.Csound.Compile         as CSD
import qualified Payasan.PSC.LilyPond.Compile       as LY
import qualified Payasan.PSC.LilyPond.OutTrans      as LY
import qualified Payasan.PSC.LilyPond.Utils         as LY
import Payasan.PSC.LilyPond.ExternalUnquote

import qualified Payasan.PSC.MIDI.Compile           as MIDI

import Payasan.PSC.Csound.Output (Value(..))


import Payasan.PSC.Repr.External.Syntax
import Payasan.PSC.Base.SyntaxCommon

import Payasan.Base.AltPitch
import Payasan.Base.Basis
import Payasan.Base.Pitch





-- MONOPHONIC

locals :: SectionInfo
locals = default_section_info

globals :: ScoreInfo
globals = default_score_info 


abc_compiler :: ABC.PartCompiler
abc_compiler = ABC.makePartCompiler lib
  where
    lib = ABC.emptyDef

compileABCPart :: FilePath -> StdPart -> IO ()
compileABCPart path part = 
   let doc = ABC.assembleOutput "Untitled" TREBLE (initialSectionInfo part) $ ABC.compilePart abc_compiler part
   in ABC.writeABCFile path doc


ly_compiler :: Anno anno => LY.PartCompiler Pitch anno
ly_compiler = LY.makePartCompiler lib
  where
    lib = LY.emptyDef { LY.transformPitch = LY.transformLyPitch_Absolute
                      , LY.pAnno = anno 
                      , LY.pPitch = LY.pitch
                      }

compileLyPart :: FilePath -> StdPart -> IO ()
compileLyPart path part = 
   let doc = LY.assembleOutput "2.18.2" "Untitled" $ LY.compilePart ly_compiler part
   in LY.writeLyFile path doc


data BowedBarAttrs = BowedBarAttrs 
    { bb_amp       :: Decimal
    , bb_pitch     :: PCPitch
    , bb_const     :: Decimal
    }
  deriving (Eq,Show)

bbEvent :: Pitch -> anno -> BowedBarAttrs
bbEvent p _ = 
    BowedBarAttrs { bb_amp      = 0.8
                  , bb_pitch    = pitchToPCPitch p
                  , bb_const    = 0.0
                  }

bbGrace :: Pitch -> BowedBarAttrs
bbGrace p = 
    BowedBarAttrs { bb_amp      = 0.6
                  , bb_pitch    = pitchToPCPitch p
                  , bb_const    = 0.0
                  }

genValues :: BowedBarAttrs -> CSD.CsoundNote
genValues (BowedBarAttrs { bb_amp      = amp
                         , bb_pitch    = pch
                         , bb_const    = konst }) = 
    CSD.CsoundNote $ [ VFloat 2 amp
                     , VFloat 2 $ realToFrac $ getPCPitch pch
                     , VFloat 1 konst 
                     ]


csd_compiler :: CSD.PartCompiler Pitch anno
csd_compiler = CSD.makePartCompiler lib
  where 
    lib = CSD.emptyDef { CSD.instrument_number    = 1
                       , CSD.make_event_body      = \p a -> genValues $ bbEvent p a
                       , CSD.make_grace_body      = genValues . bbGrace
                       }
    


compileCsoundPart :: FilePath -> StdPart -> IO ()
compileCsoundPart path part = 
   do { xplate <- CSD.readCsdTemplate "./demo/template.csd"
      ; let sco = CSD.compilePart csd_compiler part
      ; let csd = CSD.assembleOutput xplate "[|eventlist|]" sco
      ; CSD.writeCsdFile path csd
      }
   



midiEvent :: Pitch -> anno -> MIDI.MIDINote
midiEvent pch _ = 
    let midinote = pitchToMidiPitch pch in MIDI.MIDINote midinote 40 40

midiGrace :: Pitch -> MIDI.MIDINote
midiGrace pch = 
    let midinote = pitchToMidiPitch pch in MIDI.MIDINote midinote 40 40


midi_compiler :: MIDI.PartCompiler Pitch anno
midi_compiler = MIDI.makePartCompiler lib
  where
    lib = MIDI.emptyDef { MIDI.make_event_body = midiEvent
                        , MIDI.make_grace_body = midiGrace
                        }

compileMIDIPart :: FilePath -> StdPart -> IO ()
compileMIDIPart path part = 
   let midi = MIDI.assembleOutput $  MIDI.compilePart midi_compiler part
   in MIDI.writeMIDIFile path midi

section1abc :: StdSection
section1abc = unquoteABC "Phrase1abc" locals $ [abc| B4 z B B - | BB B2 z4 |]

demo01 :: IO ()
demo01 = compileABCPart "abc_output.abc" (Part { part_sections = [section1abc] })


-- Note - ElemPart reads (and ignores) beam group brackets.
-- Beams are re-synthesized in the output.
--
section1ly :: StdSection
section1ly = unquoteLyRelative "Phrase1ly" locals middle_c $ 
    [lilypond| b'2 r8 b8 b4 ~ | b8[b] b4 r2 |]

demo02 :: IO ()
demo02 = compileLyPart "ly_output.ly" (Part { part_sections = [section1ly] })

demo03 :: IO ()
demo03 = compileCsoundPart "csd_output.csd" (Part { part_sections = [section1abc] })


demo04 :: IO ()
demo04 = compileMIDIPart "midi_output.midi" (Part { part_sections = [section1abc] })




{-
demo01 :: IO ()
demo01 = shellOutABC default_shell_info $ 
    outputAsABC globals staff $ phrase01abc

demo02 :: IO ()
demo02 = shellOutLilyPond default_shell_info $ 
    outputAsLilyPond_Relative globals middle_c $ phrase01ly


-- Just two notes...
phrase02 :: StdElemPart
phrase02 = fromLilyPondWith_Relative middle_c locals $ 
    [lilypond| b'2 r8 b8 ~ b4  |]

demo03 :: IO ()
demo03 = writeAsMIDI "out/tied01.mid" phrase02



phrase10 :: StdElemPart
phrase10 = fromABCWith locals $ [abc| c G2 E2 C/2 | c |]

demo10 :: IO ()
demo10 = printAsABC default_score_info staff phrase10

demo11 :: IO ()
demo11 = writeAsMIDI "out/phrase1.mid" phrase10

demo12 :: IO ()
demo12 = printAsTabular default_score_info phrase10

demo13 :: IO ()
demo13 = printAsLinear default_score_info phrase10

-}