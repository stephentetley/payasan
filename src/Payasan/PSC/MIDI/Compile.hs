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

import Payasan.Base.AltPitch
import Payasan.Base.Duration ( Duration )

import qualified ZMidi.Core as Z                -- package: zmidi-core




import Control.Monad.IO.Class
import Data.Word
import System.FilePath



type MIDICompile a = CM a

--
-- make_event_body is wrong - it exposes chan and potentially
-- allows two NoteOns or two NoteOffs
-- 
-- There is also a dichotomy between compiling a file and 
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
    , ticks_per_quarter_note    :: !Int
    , make_event_body           :: pch -> anno -> MIDINote
    , make_grace_body           :: pch -> MIDINote
    }


-- data MidiNote = MidiNote !MidiPitch !Word8 !Word

-- make_event_body  :: pch anno -> 

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
compilePartToEventList1 def part = 
    let irsimple = fromExternal $ transDurationToSeconds part
        irflat   = fromIREventBar2 def_bar $ fromIRSimpleTile irsimple
    in return $ makeMIDIEventList $ ticksTrafo (ticks_per_quarter_note def) $ irflat
  where
    event2   = \p a -> let (MIDINote ch pch onn off) = (make_event_body def) p a
                       in (NoteOn ch pch onn, NoteOff ch pch off) 

    grace2   = \p -> let (MIDINote ch pch onn off) = (make_grace_body def) p
                     in (NoteOn ch pch onn, NoteOff ch pch off) 

    def_bar  = GenEventBody2 { genBodyFromEvent2 = event2
                             , genBodyFromGrace2 = grace2 }

    


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

