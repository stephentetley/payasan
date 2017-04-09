{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.ABC.Compile
-- Copyright   :  (c) Stephen Tetley 2016-2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Compiler for ABC.
--
--------------------------------------------------------------------------------

module Payasan.PSC.ABC.Compile
  ( 

    PartCompilerDef(..)
  , emptyDef

  , PartCompiler(..)
  , makePartCompiler

  , ABCScore

  , assembleOutput
  , writeABC

  ) where


import Payasan.PSC.ABC.Output
import Payasan.PSC.ABC.OutTrans

import Payasan.PSC.Base.SyntaxCommon
import Payasan.PSC.Base.Utils
import qualified Payasan.PSC.Repr.External.Syntax as EXT

import Payasan.Base.Duration
import Payasan.Base.Pitch




--------------------------------------------------------------------------------
-- Latest - move to PartCompiler style...


data PartCompiler = PartCompiler
    { compilePart :: forall anno. EXT.Part Pitch Duration anno -> ABCNoteList
    }


-- Clef is a mid-tune field in ABC, hence we should be able to 
-- render parts with different clefs in the same score.


data PartCompilerDef = PartCompilerDef 
    { clef_name          :: !String
    , bars_per_line     :: !Int
    } 

emptyDef :: PartCompilerDef
emptyDef = PartCompilerDef
    { clef_name         = "treble"
    , bars_per_line     = 4
    }



makePartCompiler :: PartCompilerDef -> PartCompiler
makePartCompiler lib = PartCompiler
    { compilePart = compileP
    }
  where
    cols          = bars_per_line lib
    
    compileP part = let abc_ext = specializeToABCExternal part
                        out     = makeABCNoteList cols abc_ext
                    in out


data ABCScore_ 
type ABCScore = TyDoc ABCScore_


-- TODO clef and SectionInfo should be not be exposed...
-- Should allow some user customization...
--
assembleOutput :: String -> String -> SectionInfo -> ABCNoteList -> ABCScore
assembleOutput title1 clef1 info notes = 
    TyDoc $ assembleABC (makeABCHeader title1 clef1 info) notes


writeABC :: FilePath -> ABCScore -> IO ()
writeABC path doc = writeFile path (ppRender $ extractDoc doc)