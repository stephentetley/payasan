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
  , MIDINote(..)

  , CompilerDef(..)       
  , emptyDef_

  , Compiler(..)
  , makeCompiler

  , PartCompilerDef(..)
  , emptyDef

  , PartCompiler(..)
  , makePartCompiler

  , assembleOutputAlt
  , writeMIDIFileAlt

  ) where


import Payasan.PSC.MIDI.Output
import Payasan.PSC.MIDI.Syntax

import Payasan.PSC.Repr.External.OutTransSeconds
import qualified Payasan.PSC.Repr.External.Syntax       as EXT

import Payasan.PSC.Repr.IRSimpleTile.FromExternal
import Payasan.PSC.Repr.IREventBar.FromIRSimpleTile
import Payasan.PSC.Repr.IREventFlat.FromIREventBar

import Payasan.PSC.Base.CompilerMonad

import Payasan.Base.AltPitch
import Payasan.Base.Duration ( Duration )

import qualified ZMidi.Core as Z                -- package: zmidi-core




import Control.Monad.IO.Class
import Data.Word
import System.FilePath



type MIDICompile a = CM a

-- 
-- There is a dichotomy between compiling a file and 
-- compiling a track (a file might have a number of tracks...)
--

-- | User facing abstraction of Midi NoteOn/NoteOff 
-- TODO - hide channel
--
data MIDINote = MIDINote 
    { note_chan          :: !Word8
    , note_pitch         :: !MidiPitch 
    , note_on_velocity   :: !Word8 
    , note_off_velocity  :: !Word8
    }
  deriving (Eq,Show)

data CompilerDef pch anno = CompilerDef
    { pathto_working_dir        :: !FilePath
    , outfile_name              :: !String
    , make_event_body           :: pch -> anno -> MIDINote
    , make_grace_body           :: pch -> MIDINote
    }



emptyDef_ :: CompilerDef pch anno
emptyDef_ = CompilerDef
    { pathto_working_dir        = ""
    , outfile_name              = "midi_output.midi"
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
compile1 lib part = do 
    { events <- compilePartToEventList1 lib part
    ; csd    <- assembleOutput1 lib events
    ; writeMIDIFile1 lib csd
    }

    


compilePartToEventList1 :: CompilerDef pch anno
                        -> EXT.Part pch Duration anno 
                        -> MIDICompile MIDIEventList
compilePartToEventList1 lib part = 
    let irsimple = fromExternal $ transDurationToSeconds part
        irflat   = fromIREventBar2 def_bar $ fromIRSimpleTile irsimple
    in return $ makeMIDIEventList $ ticksTrafo irflat
  where
    def_bar  = makeGenEventBody2 lib

    
makeGenEventBody2 :: CompilerDef pch anno -> GenEventBody2 pch anno MIDIEventBody
makeGenEventBody2 lib = 
    GenEventBody2 { genBodyFromEvent2 = event
                  , genBodyFromGrace2 = grace }
  where
    event p a = let (MIDINote ch pch onn off) = (make_event_body lib) p a
                in (NoteOn ch pch onn, NoteOff ch pch off) 

    grace p   = let (MIDINote ch pch onn off) = (make_grace_body lib) p
                in (NoteOn ch pch onn, NoteOff ch pch off) 


assembleOutput1 :: CompilerDef pitch anno -> MIDIEventList -> MIDICompile Z.MidiFile
assembleOutput1 _lib evts = 
    return $ midiFileFormat0 (makeZMidiTrack evts)


writeMIDIFile1 :: CompilerDef pch anno -> Z.MidiFile -> MIDICompile ()
writeMIDIFile1 lib midi =
    do { outfile <- workingFileName1 lib
       ; liftIO $ Z.writeMidi outfile midi
       ; return ()
       }


workingFileName1 :: CompilerDef pch anno -> MIDICompile FilePath
workingFileName1 lib = 
    do { root <- getWorkingDirectory (Right $ pathto_working_dir lib) 
       ; let name = outfile_name lib
       ; let outfile = root </> name
       ; return outfile
       }

--------------------------------------------------------------------------------
-- Latest work 

-- Compilation may compile more-than-one Parts which each have different
-- configurations. Need to treat Parts individually.

data PartCompiler pch anno = PartCompiler
    { compilePart :: EXT.Part pch Duration anno -> MIDIEventList
    }

data PartCompilerDef pch anno = PartCompilerDef 
    { midi_channel              :: !Int
    , make_event_body1          :: pch -> anno -> MIDINote
    , make_grace_body1          :: pch -> MIDINote
    } 

emptyDef :: PartCompilerDef pch anno
emptyDef = PartCompilerDef 
    { midi_channel              = 1
    , make_event_body1          = \_ _ -> mkErr "make_event_body1"
    , make_grace_body1          = \_ -> mkErr "make_grace_body1"
    }
  where
    mkErr ss = error $ "Must supply an implementation of " ++ ss



-- ticks_per_quarter_note is solved by always using 480
--
makePartCompiler :: PartCompilerDef pch anno -> PartCompiler pch anno
makePartCompiler lib = PartCompiler
    { compilePart = compileP
    }
  where
    gen2          = makeGenEventBody2_ lib
    compileP part = let irsimple = fromExternal $ transDurationToSeconds part
                        irflat   = fromIREventBar2 gen2 $ fromIRSimpleTile irsimple
                    in  makeMIDIEventList $ ticksTrafo irflat



makeGenEventBody2_ :: PartCompilerDef pch anno -> GenEventBody2 pch anno MIDIEventBody
makeGenEventBody2_ lib = 
    GenEventBody2 { genBodyFromEvent2 = event
                  , genBodyFromGrace2 = grace }
  where
    event p a = let (MIDINote ch pch onn off) = (make_event_body1 lib) p a
                in (NoteOn ch pch onn, NoteOff ch pch off) 

    grace p   = let (MIDINote ch pch onn off) = (make_grace_body1 lib) p
                in (NoteOn ch pch onn, NoteOff ch pch off) 



assembleOutputAlt :: MIDIEventList -> Z.MidiFile
assembleOutputAlt evts = midiFileFormat0 (makeZMidiTrack evts)

writeMIDIFileAlt :: FilePath -> Z.MidiFile -> IO ()
writeMIDIFileAlt path midi = Z.writeMidi path midi
       
       

