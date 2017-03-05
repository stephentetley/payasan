{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.LilyPond.Compile
-- Copyright   :  (c) Stephen Tetley 2016-2017
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

  , PartCompilerDef(..)
  , emptyDef1

  , PartCompiler(..)
  , makePartCompiler

  , LyFile

  , writeLyFile

  ) where


import Payasan.PSC.LilyPond.Common hiding ( middle_c )
import Payasan.PSC.LilyPond.OutTrans
import Payasan.PSC.LilyPond.SimpleOutput
import Payasan.PSC.LilyPond.Utils

import Payasan.PSC.Base.CompilerMonad
import Payasan.PSC.Base.SyntaxCommon
import Payasan.PSC.Base.Utils
import Payasan.PSC.Repr.External.Syntax
import qualified Payasan.PSC.Repr.External.Syntax as EXT

import Payasan.Base.Duration
import Payasan.Base.Pitch 

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
                       => CompilerDef -> StdPart1 anno -> LyCompile LyNoteList
compilePartToNoteList1 def p = do 
    { p1 <- rebeam p 
    ; p2 <- normalize p1
    ; let info = initialSectionInfo p
    ; p3 <- return $ makeLyNoteList outDef info p2
    ; return p3
    }
  where
    outDef = LyOutputDef { printPitch = pitch, printAnno = anno }
    normalize = return . translateToLyPartOut_Relative middle_c
    rebeam s = if (recalc_beams def) then (addBeams <=< delBeams) s else return s 
                  
    -- TEMP
    addBeams = return 
    delBeams = return

    
assembleOutput1 :: CompilerDef -> LyNoteList -> Doc
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


--------------------------------------------------------------------------------
-- PartCompiler

data PartCompiler ext_pch anno = PartCompiler
    { compilePart :: EXT.Part ext_pch Duration anno -> LyNoteList
    }

-- Does percussion music need a clef?
-- (Also should clef be an open set?)
data PartCompilerDef ext_pch ly_pch anno = PartCompilerDef 
    { pPitch            :: ly_pch -> Doc               -- temp wrong
    , pAnno             :: anno -> Doc
    , transformPitch    :: 
          forall drn. EXT.Part ext_pch drn anno -> EXT.Part ly_pch drn anno
    , clefm             :: Maybe Clef
    , recalc_beams1     :: !Bool
    } 


emptyDef1 :: PartCompilerDef ext_pch ly_pch anno
emptyDef1 = PartCompilerDef
    { pPitch            = mkErr "pPitch"
    , pAnno             = mkErr "pAnno"
    , transformPitch    = mkErr "transformPitch"
    , clefm             = Just TREBLE
    , recalc_beams1     = False
    }
  where
    mkErr ss = error $ "Must supply an implementation of " ++ ss


-- Does LilyPond merit a further Repr?
-- Need to separate pitch changing and duration changing traversals
-- so we can expose pitch changing to the user.

makePartCompiler :: forall ext_pch ly_pch anno.
                    PartCompilerDef ext_pch ly_pch anno -> PartCompiler ext_pch anno
makePartCompiler lib = PartCompiler
    { compilePart = compileP
    }
  where
    rebeam s  = if recalc_beams1 lib then (addBeams . delBeams) s else s 
      where
        addBeams  = id  -- TEMP
        delBeams  = id  -- TEMP

    pchTrafo :: Part ext_pch drn anno -> Part ly_pch drn anno
    pchTrafo  = transformPitch lib

    outDef    :: LyOutputDef ly_pch anno
    outDef    = LyOutputDef { printPitch = pPitch lib 
                            , printAnno  = pAnno lib }

    compileP :: Part ext_pch Duration anno -> LyNoteList
    compileP part = let info    = initialSectionInfo part
                        ext1    = rebeam part
                        extly   = transformLyNoteLength $ pchTrafo ext1
                        out     = makeLyNoteList outDef info extly
                    in out



data LyFile_ 
type LyFile = TyDoc LyFile_




writeLyFile :: FilePath -> LyFile -> IO ()
writeLyFile path doc = 
    writeFile path (ppRender $ extractDoc doc)
