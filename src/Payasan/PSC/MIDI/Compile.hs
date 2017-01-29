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

import Payasan.PSC.Repr.IRSimpleTile.FromExternal
import Payasan.PSC.Repr.IREventBar.FromIRSimpleTile
import Payasan.PSC.Repr.IREventFlat.FromIREventBar

import Payasan.PSC.Base.CompilerMonad

import Payasan.Base.Duration ( Duration )

import qualified ZMidi.Core as Z                -- package: zmidi-core



import Control.Monad.IO.Class
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
    , make_event_body           = \_ _ -> mkErr "make_event_body"
    , make_grace_body           = \_ -> mkErr "make_grace_body"
    }
  where
    mkErr ss = error $ "Must supply an implementation of " ++ ss


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
                        -> MIDICompile MIDIEventList
compilePartToEventList1 def p = 
    let def_bar  = GenEventBody2 { genBodyFromEvent2 = make_event_body def
                                 , genBodyFromGrace2 = make_grace_body def }          
        irsimple = fromExternal $ transDurationToSeconds p
        irflat   = fromIREventBar2 def_bar $ fromIRSimpleTile irsimple
    in return $ makeMIDIEventList $ ticksTrafo (ticks_per_quarter_note def) $ irflat



assembleOutput1 :: CompilerDef pitch anno -> MIDIEventList -> MIDICompile Z.MidiFile
assembleOutput1 def evts = 
    return $ midiFileFormat0 (ticks_per_quarter_note def) (makeZMidiTrack evts)


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

