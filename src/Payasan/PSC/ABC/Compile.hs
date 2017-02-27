{-# LANGUAGE RankNTypes                 #-}
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

    CompilerDef(..)       
  , emptyDef

  , Compiler(..)
  , makeCompiler


  , PartCompilerDef(..)
  , emptyDef2

  , PartCompiler(..)
  , makePartCompiler

  , assembleOutput
  , writeABCFile

  ) where


import Payasan.PSC.ABC.Output
import Payasan.PSC.ABC.OutTrans

import Payasan.PSC.Base.CompilerMonad
import Payasan.PSC.Base.SyntaxCommon
import Payasan.PSC.Base.Utils
import Payasan.PSC.Repr.External.Syntax
import qualified Payasan.PSC.Repr.External.Syntax as EXT

import Payasan.Base.Duration
import Payasan.Base.Pitch

import Text.PrettyPrint.HughesPJ hiding ( Mode )       -- package: pretty


import Control.Monad
import Control.Monad.IO.Class
import System.FilePath

-- Public env exposed to users...

data CompilerDef = CompilerDef
    { pathto_working_dir     :: !String
    , outfile_name           :: !String
    , title                  :: !String
    , clef                   :: !Clef
    , bars_per_line          :: !Int
    , recalc_beams           :: !Bool
    }



emptyDef :: CompilerDef
emptyDef = CompilerDef
    { pathto_working_dir        = ""
    , outfile_name              = "abcoutput.abc"
    , title                     = "Tune 1"
    , clef                      = TREBLE
    , bars_per_line             = 4
    , recalc_beams              = True
    }


type ABCCompile a = CM a


-- Note - there is very little user variation that Payasan 
-- should support when generating ABC: 
-- A user might want extra header fields (some are mandatory)
-- and number of bars per line should be allowed but otherwise 
-- ABC output is very regimented.


-- Note - clef output for abc is part of the 'K' key field, there
-- isn't a simple "clef field".
--
    
data Compiler = Compiler
   { compile :: forall anno. StdPart1 anno -> IO ()
   }

makeCompiler :: CompilerDef -> Compiler
makeCompiler env = 
    Compiler { compile = \part -> prompt (compile1 env part)  >> return ()
             }



-- Note - initial section info can fallback to sensible defaults 
-- for an empty score
compile1 :: CompilerDef -> StdPart1 anno -> ABCCompile ()
compile1 def part = do 
    { let info = initialSectionInfo part
    ; let header = makeHeader1 def info
    ; notes  <- compilePartToNoteList1 def part
    ; let abc = assembleABC header notes 
    ; writeABCFile1 def (ppRender abc)
    }


-- | Do we want to recalc beams (probably...)

compilePartToNoteList1 :: CompilerDef -> StdPart1 anno -> ABCCompile ABCNoteListDoc
compilePartToNoteList1 def p = do 
    { p1 <- rebeam p 
    ; p2 <- normalize p1
    ; let info = initialSectionInfo p
    ; p3 <- return $ makeABCNoteListDoc 4 info p2
    ; return p3
    }
  where
    normalize = return . translateToABCPartOut
    rebeam s = if recalc_beams def then (addBeams <=< delBeams) s else return s 
                 
    -- TEMP
    addBeams = return 
    delBeams = return


makeHeader1 :: CompilerDef -> SectionInfo -> ABCHeader
makeHeader1 def info = 
    makeHeader (title def) (clef def) info

                      

    
-- | ABC has already been rendered to String.
--
writeABCFile1 :: CompilerDef -> String -> ABCCompile ()
writeABCFile1 def abc = 
    do { outfile <- workingFileName1 def
       ; liftIO $ writeFile outfile abc
       ; return ()
       }
       

workingFileName1 :: CompilerDef -> ABCCompile String
workingFileName1 def = 
    do { root <- getWorkingDirectory (Right $ pathto_working_dir def)
       ; let name = outfile_name def
       ; let outfile = root </> name
       ; return outfile
       }

--------------------------------------------------------------------------------
-- Latest - move to PartCompiler style...


data PartCompiler = PartCompiler
    { compilePart :: forall anno. EXT.Part Pitch Duration anno -> ABCNoteListDoc
    }


-- Clef is a mid-tune field in ABC, hence we should be able to 
-- render parts with different clefs in the same score.


data PartCompilerDef = PartCompilerDef 
    { clef1             :: !Clef
    , bars_per_line1    :: !Int
    } 

emptyDef2 :: PartCompilerDef
emptyDef2 = PartCompilerDef
    { clef1             = TREBLE
    , bars_per_line1    = 4
    }



makePartCompiler :: PartCompilerDef -> PartCompiler
makePartCompiler lib = PartCompiler
    { compilePart = compileP
    }
  where
    cols          = bars_per_line1 lib
    
    compileP part = let info    = initialSectionInfo part
                        abc_ext = specializeToABCExternal part
                        out     = makeABCNoteListDoc cols info abc_ext
                    in out


-- TODO clef and SectionInfo should be not be exposed...
--
assembleOutput :: String -> Clef -> SectionInfo -> ABCNoteListDoc -> Doc
assembleOutput title1 clef1 info notes = 
    assembleABC (makeHeader title1 clef1 info) notes


writeABCFile :: FilePath -> Doc -> IO ()
writeABCFile path doc = 
    writeFile path (ppRender doc)