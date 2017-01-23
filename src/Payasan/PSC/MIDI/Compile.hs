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
import Payasan.PSC.MIDI.PrimitiveSyntax

import Payasan.PSC.Repr.External.OutTransSeconds

import Payasan.PSC.Repr.IREventFlat.Syntax
import Payasan.PSC.Repr.IRSimpleTile.FromExternal
import Payasan.PSC.Repr.IREventBar.FromIRSimpleTile
import Payasan.PSC.Repr.IREventFlat.FromIREventBar

import Payasan.PSC.Base.CompilerMonad
import Payasan.PSC.Base.SyntaxCommon
import Payasan.PSC.Base.Utils
import Payasan.PSC.Repr.External.Syntax



import Payasan.Base.Pitch ( Pitch )

import Text.PrettyPrint.HughesPJ                -- package: pretty

import Control.Monad
import Control.Monad.IO.Class
import Data.Data
import System.FilePath



type MIDICompile a = CM a


data CompilerDef pch anno body = CompilerDef
    { pathto_working_dir        :: !FilePath
    , outfile_name              :: !String
    , make_event_body           :: pch -> anno -> body
    , make_grace_body           :: pch -> body
    }



emptyDef :: CompilerDef pch anno body
emptyDef = CompilerDef
    { pathto_working_dir        = ""
    , outfile_name              = "midi_output.midi"
    }
  

data Compiler anno = Compiler
   { compile :: StdPart1 anno -> IO ()
   
   }


makeCompiler :: CompilerDef pch anno body -> Compiler anno
makeCompiler env = 
    Compiler { compile = \part -> prompt (compile1 env part)  >> return ()
             }




compile1 :: CompilerDef pch anno body -> StdPart1 anno -> MIDICompile ()
compile1 _ _ = error "compile1"

{-
compile1 def part = do 
    { events <- compilePartToEventList1 def part
    ; csd <- assembleOutput1 def events
    ; writeCsdFile1 def csd
    }
-}
    


compilePartToEventList1 :: CompilerDef Pitch anno body
                        -> StdPart1 anno 
                        -> MIDICompile ()
compilePartToEventList1 def p = 
    let def_bar  = GenEventBody { genBodyFromEvent = make_event_body def
                                , genBodyFromGrace = make_grace_body def }          
        irsimple = fromExternal $ transDurationToSeconds p
        irflat   = fromIREventBar def_bar $ fromIRSimpleTile irsimple
    in return ()


