{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.MIDI.Compile
-- Copyright   :  (c) Stephen Tetley 2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Compiler for MIDI.
--
--------------------------------------------------------------------------------

module Payasan.PSC.MIDI.Compile
  ( 
    
    MIDICompile
  , CompilerDef(..)       
  , emptyDef

  , Compiler(..)
--  , makeCompiler


  ) where


import Payasan.PSC.MIDI.Output
import Payasan.PSC.MIDI.PrimitiveSyntax

import Payasan.PSC.Base.CompilerMonad
import Payasan.PSC.Base.SyntaxCommon
import Payasan.PSC.Base.Utils
import Payasan.PSC.Repr.External.Syntax



import Payasan.Base.Pitch ( middle_c )

import Text.PrettyPrint.HughesPJ                -- package: pretty

import Control.Monad
import Control.Monad.IO.Class
import Data.Data
import System.FilePath



type MIDICompile a = CM a


data CompilerDef = CompilerDef
    { pathto_working_dir        :: !FilePath
    , outfile_name              :: !String
    }



emptyDef :: CompilerDef
emptyDef = CompilerDef
    { pathto_working_dir        = ""
    , outfile_name              = "midi_output.midi"
    }
  

data Compiler anno = Compiler
   { compile :: StdPart1 anno -> IO ()
   
   }


makeCompiler :: CompilerDef -> Compiler anno
makeCompiler env = 
    Compiler { compile = \part -> prompt (compile1 env part)  >> return ()
             }




compile1 :: CompilerDef -> StdPart1 anno -> MIDICompile ()
compile1 _ _ = error "compile1"

{-
compile1 def part = do 
    { events <- compilePartToEventList1 def part
    ; csd <- assembleOutput1 def events
    ; writeCsdFile1 def csd
    }
-}
    

{-
compilePartToEventList1 :: CompilerDef
                        -> StdPart1 anno 
                        -> MIDICompile ()
compilePartToEventList1 def p = 
    let def_bar  = GenEventAttrs { genAttrsFromEvent = make_event_attrs def
                                 , genAttrsFromGrace = make_grace_attrs def }          
        irsimple = fromExternal $ transDurationToSeconds p
        irflat   = fromIREventBar def_bar $ fromIRSimpleTile irsimple
    in return ()
-}


{-

-- | Do we want to recalc beams (probably...)

compilePartToNoteList :: StdPart1 anno -> LyCompile LyNoteListDoc
compilePartToNoteList p = do 
    { p1 <- rebeam p 
    ; p2 <- normalize p1
    ; let info = initialSectionInfo p
    ; p3 <- error "TODO" -- rewrite (makeLyNoteListDoc p2) () (stateZero info)
    ; return p3
    }
  where
    normalize = return . translateToLyPartOut_Relative middle_c
    rebeam s = do { ans <- asksUE ly_recalc_beams
                  ; if ans then (addBeams <=< delBeams) s else return s 
                  }
    -- TEMP
    addBeams = return 
    delBeams = return

    
assembleOutput :: SectionInfo -> LyNoteListDoc -> LyCompile Doc
assembleOutput info notes = do 
    { (title, clef) <- tuneConfig
    ; return $ error "TODO" -- assembleLy (makeHeader title clef info) notes
    }
  where
    tuneConfig = (,) <$> asksUE ly_tune_title <*> asksUE ly_clef 
                      
                      

    
-- | Ly has already been rendered to String.
--
writeLyFile :: String -> LyCompile ()
writeLyFile abc = 
    do { root <- getTempDirectory =<< asksUE ly_cwd_loc
       ; name <- asksUE ly_outfile_name
       ; let outfile = root </> name
       ; liftIO $ writeFile outfile abc
       ; return ()
       }

       

writeMIDIFile :: WriteOutput Track -> Track -> MIDICompile ()
writeMIDIFile def trk = 
    do { root <- getTempDirectory =<< asksUE ly_cwd_loc
       ; name <- asksUE ly_outfile_name
       ; let outfile = root </> name
       ; liftIO $ (writeOutput def) outfile trk
       ; return ()
       }
       

data WriteOutput o = WriteOutput { writeOutput :: FilePath -> o -> IO () }

midiOutput :: WriteOutput Track
midiOutput = WriteOutput { writeOutput = \outfile trk-> writeMF1 outfile [trk] }

-}