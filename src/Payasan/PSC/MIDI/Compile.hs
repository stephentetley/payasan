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
    MIDINote(..)


  , PartCompilerDef(..)
  , emptyDef

  , PartCompiler(..)
  , makePartCompiler

  , assembleOutput
  , writeMIDIFile

  ) where


import Payasan.PSC.MIDI.Output
import Payasan.PSC.MIDI.Syntax

import Payasan.PSC.Repr.External.OutTransSeconds
import qualified Payasan.PSC.Repr.External.Syntax       as EXT

import Payasan.PSC.Repr.IRSimpleTile.FromExternal
import Payasan.PSC.Repr.IREventBar.FromIRSimpleTile
import Payasan.PSC.Repr.IREventFlat.FromIREventBar


import Payasan.Base.AltPitch
import Payasan.Base.Duration ( Duration )

import qualified ZMidi.Core as Z                -- package: zmidi-core



import Data.Word



data MIDINote = MIDINote 
    { note_pitch         :: !MidiPitch 
    , note_on_velocity   :: !Word8 
    , note_off_velocity  :: !Word8
    }
  deriving (Eq,Show)



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
    gen2          = makeGenEventBody2 lib
    compileP part = let irsimple = fromExternal $ transDurationToSeconds part
                        irflat   = fromIREventBar2 gen2 $ fromIRSimpleTile irsimple
                    in  makeMIDIEventList $ ticksTrafo irflat



makeGenEventBody2 :: PartCompilerDef pch anno -> GenEventBody2 pch anno MIDIEventBody
makeGenEventBody2 lib = 
    GenEventBody2{ genBodyFromEvent2 = event
                  , genBodyFromGrace2 = grace }
  where
    chan      = fromIntegral $ midi_channel lib
    event p a = let (MIDINote pch onn off) = (make_event_body1 lib) p a
                in (NoteOn chan pch onn, NoteOff chan pch off) 

    grace p   = let (MIDINote pch onn off) = (make_grace_body1 lib) p
                in (NoteOn chan pch onn, NoteOff chan pch off) 



assembleOutput :: MIDIEventList -> Z.MidiFile
assembleOutput evts = midiFileFormat0 (makeZMidiTrack evts)

writeMIDIFile :: FilePath -> Z.MidiFile -> IO ()
writeMIDIFile path midi = Z.writeMidi path midi
       
       

