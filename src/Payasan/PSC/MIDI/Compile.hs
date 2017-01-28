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
  , makeCompiler


  ) where


import Payasan.PSC.MIDI.Output
import Payasan.PSC.MIDI.Syntax

import Payasan.PSC.Repr.External.OutTransSeconds
import qualified Payasan.PSC.Repr.External.Syntax       as EXT

import Payasan.PSC.Repr.IREventFlat.Syntax
import Payasan.PSC.Repr.IRSimpleTile.FromExternal
import Payasan.PSC.Repr.IREventBar.FromIRSimpleTile
import Payasan.PSC.Repr.IREventFlat.FromIREventBar

import Payasan.PSC.Base.CompilerMonad
import Payasan.PSC.Base.SyntaxCommon
import Payasan.PSC.Base.Utils
import Payasan.PSC.Repr.External.Syntax

import Payasan.Base.Duration ( Duration )
import Payasan.Base.Pitch ( Pitch )

import qualified ZMidi.Core as Z                -- package: zmidi-core


import Text.PrettyPrint.HughesPJ                -- package: pretty

import Control.Monad
import Control.Monad.IO.Class
import Data.Data
import System.FilePath



type MIDICompile a = CM a


data CompilerDef pch anno = CompilerDef
    { pathto_working_dir        :: !FilePath
    , outfile_name              :: !String
    , ticks_per_quarter_note    :: !Int
    , make_event_body           :: pch -> anno -> (MIDIEventBody, MIDIEventBody)
    , make_grace_body           :: pch -> (MIDIEventBody, MIDIEventBody)
    }



emptyDef :: CompilerDef pch anno
emptyDef = CompilerDef
    { pathto_working_dir        = ""
    , outfile_name              = "midi_output.midi"
    , ticks_per_quarter_note    = 480
    }
  

data Compiler pch anno = Compiler
   { compile :: EXT.Part pch Duration anno -> IO ()
   
   }


makeCompiler :: CompilerDef pch anno -> Compiler pch anno
makeCompiler env = 
    Compiler { compile = \part -> prompt (compile1 env part)  >> return ()
             }




compile1 :: CompilerDef pch anno -> EXT.Part pch Duration anno -> MIDICompile ()
compile1 def part = do 
    { events <- compilePartToEventList1 def part
    ; csd    <- assembleOutput1 def events
    ; writeMIDIFile1 def csd
    }

    


compilePartToEventList1 :: CompilerDef pch anno
                        -> EXT.Part pch Duration anno 
                        -> MIDICompile (MIDIPart AbsTicks)
compilePartToEventList1 def p = 
    let def_bar  = GenEventBody2 { genBodyFromEvent2 = make_event_body def
                                 , genBodyFromGrace2 = make_grace_body def }          
        irsimple = fromExternal $ transDurationToSeconds p
        irflat   = fromIREventBar2 def_bar $ fromIRSimpleTile irsimple
    in return $ ticksTrafo (ticks_per_quarter_note def) $ irflat



assembleOutput1 :: CompilerDef pitch anno -> MIDIPart AbsTicks -> MIDICompile Z.MidiFile
assembleOutput1 def p = undefined
{-
    let scotext = TEXT.pack $ ppRender $ extractDoc sco
    in do { xcsd <- readFileCM (pathto_csd_template def)
          ; return $ TEXT.replace (TEXT.pack $ template_anchor def) scotext xcsd
          }
-}


writeMIDIFile1 :: CompilerDef pch anno -> Z.MidiFile -> MIDICompile ()
writeMIDIFile1 def midi =
    do { outfile <- workingFileName1 def
       ; liftIO $ Z.writeMidi outfile midi
       ; return ()
       }


workingFileName1 :: CompilerDef pch anno -> MIDICompile FilePath
workingFileName1 def = 
    do { root <- getWorkingDirectory (Right $ pathto_working_dir def) 
       ; let name = outfile_name def
       ; let outfile = root </> name
       ; return outfile
       }

