{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module Demo01 where

import qualified Payasan.PSC.ABC.Compile            as ABC
import Payasan.PSC.ABC.ExternalUnquote

import qualified Payasan.PSC.Csound.Compile         as CSD
import qualified Payasan.PSC.LilyPond.Compile       as LY
import Payasan.PSC.LilyPond.ExternalUnquote

import qualified Payasan.PSC.MIDI.Compile           as MIDI

import Payasan.PSC.Csound.Output (Value(..))
import Payasan.PSC.MIDI.Syntax


import Payasan.PSC.Repr.External.Syntax
import Payasan.PSC.Base.SyntaxCommon

import Payasan.Base.AltPitch
import Payasan.Base.Basis
import Payasan.Base.Duration
import Payasan.Base.Pitch

import Text.PrettyPrint.HughesPJClass



-- MONOPHONIC

locals :: SectionInfo
locals = default_section_info

globals :: ScoreInfo
globals = default_score_info 


abc_compiler :: ABC.Compiler
abc_compiler = ABC.makeCompiler def
  where 
    def = ABC.emptyDef { ABC.pathto_working_dir     = ""
                       , ABC.outfile_name           = "abc_output.abc"
                       }

compileABC :: StdPart -> IO ()
compileABC = ABC.compile abc_compiler


ly_compiler :: Anno anno => LY.Compiler anno
ly_compiler = LY.makeCompiler def
  where 
    def = LY.emptyDef { LY.pathto_working_dir     = ""
                      , LY.outfile_name           = "ly_output.ly"
                      }

compileLy :: StdPart -> IO ()
compileLy = LY.compile ly_compiler




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

-- Demanding users generate Istmts is wrong...
genValues :: BowedBarAttrs -> [Value]
genValues (BowedBarAttrs { bb_amp      = amp
                         , bb_pitch    = pch
                         , bb_const    = konst }) = 
    [VFloat 2 amp, VFloat 2 $ realToFrac $ getPCPitch pch, VFloat 1 konst ]


csd_compiler :: CSD.Compiler anno
csd_compiler = CSD.makeCompiler def
  where 
    def = CSD.emptyDef { CSD.pathto_working_dir   = ""
                       , CSD.outfile_name         = "csd_output.csd"
                       , CSD.make_event_body      = bbEvent
                       , CSD.make_grace_body      = bbGrace
                       , CSD.make_values          = genValues
                       }
    


   

compileCsd :: StdPart -> IO ()
compileCsd = CSD.compile csd_compiler

-- TODO we shouldn't be "allocating" channel here...

midiEvent :: Pitch -> anno -> MIDI.MIDINote
midiEvent pch _ = 
    let midinote = pitchToMidiPitch pch in MIDI.MIDINote 1 midinote 40 40

midiGrace :: Pitch -> MIDI.MIDINote
midiGrace pch = 
    let midinote = pitchToMidiPitch pch in MIDI.MIDINote 1 midinote 40 40

midi_compiler_ :: MIDI.Compiler Pitch anno
midi_compiler_ = MIDI.makeCompiler def
  where
    def = MIDI.emptyDef_ { MIDI.outfile_name    = "midi_output.midi"
                         , MIDI.make_event_body = midiEvent
                         , MIDI.make_grace_body = midiGrace
                         }


compileMIDI :: StdPart -> IO ()
compileMIDI = MIDI.compile midi_compiler_

midi_compiler1 :: MIDI.PartCompiler Pitch anno
midi_compiler1 = MIDI.makePartCompiler lib
  where
    lib = MIDI.emptyDef { MIDI.make_event_body1 = midiEvent
                        , MIDI.make_grace_body1 = midiGrace
                        }

compileMIDIPart :: FilePath -> StdPart -> IO ()
compileMIDIPart path part = 
   let midi = MIDI.assembleOutputAlt $  MIDI.compilePart midi_compiler1 part
   in MIDI.writeMIDIFileAlt path midi

section1abc :: StdSection
section1abc = unquoteABC "Phrase1abc" locals $ [abc| B4 z B B - | BB B2 z4 |]

demo01 :: IO ()
demo01 = compileABC (Part { part_sections = [section1abc] })


-- Note - ElemPart reads (and ignores) beam group brackets.
-- Beams are re-synthesized in the output.
--
section1ly :: StdSection
section1ly = unquoteLyRelative "Phrase1ly" locals middle_c $ 
    [lilypond| b'2 r8 b8 b4 ~ | b8[b] b4 r2 |]

demo02 :: IO ()
demo02 = compileLy (Part { part_sections = [section1ly] })

demo03 :: IO ()
demo03 = compileCsd (Part { part_sections = [section1abc] })


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