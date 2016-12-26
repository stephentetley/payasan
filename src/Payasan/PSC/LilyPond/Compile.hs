{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.LilyPond.Compile
-- Copyright   :  (c) Stephen Tetley 2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Compiler for LilyPond.
--
--------------------------------------------------------------------------------

module Payasan.PSC.LilyPond.Compile
  ( 
    
    CompilerDef(..)       
  , emptyDef

  , Compiler(..)
  , makeCompiler

  ) where


import Payasan.PSC.LilyPond.OutTrans
import Payasan.PSC.LilyPond.SimpleOutput
import Payasan.PSC.LilyPond.Utils

import Payasan.PSC.Base.CompilerMonad
import Payasan.PSC.Base.SyntaxCommon
import Payasan.PSC.Base.Utils
import Payasan.PSC.Repr.External.Syntax

import Payasan.Base.Pitch ( middle_c )

import Text.PrettyPrint.HughesPJ               -- package: pretty

import Control.Monad
import Control.Monad.IO.Class

import System.FilePath



-- TO CONSIDER
-- There is not much variation (in form) between this and the 
-- ABC compiler. Can the structure of _compilation_ go in the 
-- compiler monad?
-- Or will Csound and MIDI be too different?




-- Note - there is a lot of user variation that a LilyPond 
-- compiler might want to support.

  

-- | Note - a user might not want to print clef and title 
-- at all...
--
data CompilerDef = CompilerDef
    { pathto_working_dir        :: !FilePath
    , ly_version_number         :: !String
    , tune_title                :: !String   -- TODO over determining of header...
    , clef                      :: !Clef     -- ditto
    , outfile_name              :: !String
    , recalc_beams              :: !Bool
    }
  
emptyDef :: CompilerDef
emptyDef = CompilerDef
    { pathto_working_dir        = ""
    , ly_version_number         = "2.18.2"
    , tune_title                = "Tune 1"
    , clef                      = TREBLE
    , outfile_name              = "output.ly"
    , recalc_beams              = False    -- default should really be True once we have bits in place again
    }    
    

data Compiler anno = Compiler
   { compile :: StdPart1 anno -> IO ()
   }

makeCompiler :: Anno anno => CompilerDef -> Compiler anno
makeCompiler env = 
    Compiler { compile = \part -> prompt (compile1 env part)  >> return ()
             }

type LyCompile a = CM a



compile1 :: Anno anno => CompilerDef -> StdPart1 anno -> LyCompile ()
compile1 def part = do 
    { notes <- compilePartToNoteList1 def part
    ; ly <- return $ assembleOutput1 def notes
    ; writeLyFile1 def (ppRender ly)
    }


-- | Do we want to recalc beams (probably...)

compilePartToNoteList1 :: Anno anno 
                       => CompilerDef -> StdPart1 anno -> LyCompile LyNoteListDoc
compilePartToNoteList1 def p = do 
    { p1 <- rebeam p 
    ; p2 <- normalize p1
    ; let info = initialSectionInfo p
    ; p3 <- return $ makeLyNoteListDoc outDef info p2
    ; return p3
    }
  where
    outDef = LyOutputDef { printPitch = pitch, printAnno = anno }
    normalize = return . translateToLyPartOut_Relative middle_c
    rebeam s = if (recalc_beams def) then (addBeams <=< delBeams) s else return s 
                  
    -- TEMP
    addBeams = return 
    delBeams = return

    
assembleOutput1 :: CompilerDef -> LyNoteListDoc -> Doc
assembleOutput1 def notes = do 
    assembleLy (makeHeader (ly_version_number def) (tune_title def)) notes
                      
                      

    
-- | Ly has already been rendered to String.
--
writeLyFile1 :: CompilerDef -> String -> LyCompile ()
writeLyFile1 def ly = 
    do { outfile <- workingFileName1 def
       ; liftIO $ writeFile outfile ly
       ; return ()
       }

workingFileName1 :: CompilerDef -> LyCompile String
workingFileName1 def = 
    do { root <- getWorkingDirectory (Right $ pathto_working_dir def) 
       ; let name = outfile_name def
       ; let outfile = root </> name
       ; return outfile
       }
