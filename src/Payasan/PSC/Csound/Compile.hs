{-# LANGUAGE EmptyDataDecls             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Csound.Compile
-- Copyright   :  (c) Stephen Tetley 2016-2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Compiler for Csound.
--
--------------------------------------------------------------------------------

module Payasan.PSC.Csound.Compile
  ( 

    CsoundNote(..)

  , PartCompilerDef(..)
  , emptyDef

  , PartCompiler(..)
  , makePartCompiler


  , CsdTemplate
  , CsdFile

  , readCsdTemplate
  , assembleOutput
  , writeCsdFile

  ) where


import Payasan.PSC.Csound.Output

import Payasan.PSC.Repr.External.OutTransSeconds
import qualified Payasan.PSC.Repr.External.Syntax as EXT
import Payasan.PSC.Repr.IRSimpleTile.FromExternal
import Payasan.PSC.Repr.IREventBar.FromIRSimpleTile
import Payasan.PSC.Repr.IREventFlat.FromIREventBar

import Payasan.PSC.Base.SyntaxCommon
import Payasan.PSC.Base.Utils

import Payasan.Base.Duration


import qualified Data.Text              as TEXT
import qualified Data.Text.IO           as TEXT




--------------------------------------------------------------------------------
-- Latest cf. MIDI


-- How should Pitch be exposed to users?
-- Csound can have different representations of pitch in the 
-- output (frequency, pitch class, ...)
--
-- For MIDI the user is obliged to supply the translation from 
-- pch to MIDIPitch. However, it feels like Csound would need
-- two steps - translate to some user required pitch type, and 
-- pretty print this pitch type to the required number of decimal 
-- places.
--
-- This makes the implementation uglier.
-- (We want a concrete pch value rather than going straight to
-- a Doc so we can have ellipsis for subsequent equal values).
-- 
-- A mid-way point is to represent pch as a Csound value, this
-- potentielly allows (erroneous) mixed representations (String, 
-- Bool) but removes the annoying extra complexity of parametric pch.
-- 
-- Also the user must specify the order of fields - whilst pitch
-- is commonly first there is no guarantee and the backend cannot 
-- know the order 
-- 
data CsoundNote = CsoundNote 
    { note_values        :: [Value]  -- TODO this leaks ellipsis
    }
  deriving (Eq,Show)


data PartCompiler pch anno = PartCompiler
    { compilePart :: EXT.Part pch Duration anno -> CsoundEventListDoc
    }





data PartCompilerDef pch anno = PartCompilerDef
    { instrument_number         :: !Int
    , make_event_body           :: pch -> anno -> CsoundNote
    , make_grace_body           :: pch -> CsoundNote
    , column_formats            :: ColumnFormats
    }



emptyDef :: PartCompilerDef pch anno
emptyDef = PartCompilerDef 
    { instrument_number         = 1
    , make_event_body           = \_ _ -> mkErr "make_event_body"
    , make_grace_body           = \_ -> mkErr "make_grace_body"
    , column_formats            = ColumnFormats { inst_colwidth  = 7
                                                , time_colformat = (7,3)
                                                , other_widths   = [6,6,6] }
    }
  where
    mkErr ss = error $ "Must supply an implementation of " ++ ss


makePartCompiler :: PartCompilerDef pch anno -> PartCompiler pch anno
makePartCompiler lib = PartCompiler
    { compilePart = compileP
    }
  where
    gen_bar  = makeGenEventBody lib

    gen_flat = GenCsoundOutput { instr_number   = instrument_number lib
                               , column_specs   = column_formats lib }

    compileP part = let irsimple = fromExternal $ transDurationToSeconds part
                        irflat   = fromIREventBar gen_bar $ fromIRSimpleTile irsimple
                    in makeCsdEventListDoc gen_flat irflat


makeGenEventBody :: PartCompilerDef pch anno -> GenEventBody pch anno [Value]
makeGenEventBody lib = 
    GenEventBody { genBodyFromEvent = event
                 , genBodyFromGrace = grace }
  where
    event p a = let (CsoundNote vals) = (make_event_body lib) p a in vals

    grace p   = let (CsoundNote vals) = (make_grace_body lib) p in vals


data CsdTemplate_ 
type CsdTemplate = TyText CsdTemplate_

data CsdFile_ 
type CsdFile = TyText CsdFile_


readCsdTemplate :: FilePath -> IO CsdTemplate
readCsdTemplate path = TyText <$> TEXT.readFile path


assembleOutput :: CsdTemplate
               -> String
               -> CsoundEventListDoc 
               -> CsdFile
assembleOutput template anchor sco = 
    let scotext = TEXT.pack $ ppRender $ extractDoc sco
    in TyText $ TEXT.replace (TEXT.pack anchor) scotext (extractText template)
       

writeCsdFile :: FilePath -> CsdFile -> IO ()
writeCsdFile path csd = TEXT.writeFile path (extractText csd)