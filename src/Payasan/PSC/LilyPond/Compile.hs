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
    
    LyCompile

  , compile

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


type LyCompile a = CM LyEnv a


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
data LyEnv = LyEnv 
    { ly_version_number         :: !String
    , ly_tune_title             :: !String   -- TODO over determining of header...
    , ly_clef                   :: !Clef     -- ditto
    , ly_outfile_name           :: !String
    , ly_recalc_beams           :: !Bool
    }
  
env_zero :: LyEnv
env_zero = LyEnv 
    { ly_version_number         = "2.18.2"
    , ly_tune_title             = "Tune 1"
    , ly_clef                   = TREBLE
    , ly_outfile_name           = "output.ly"
    , ly_recalc_beams           = False    -- default should really be True once we have bits in place again
    }    
    


compile :: Anno anno => StdPart1 anno -> IO ()
compile part = runCM env_zero (compile1 part) >>= \ans -> case ans of
    Left err -> putStrLn err
    Right _ -> return ()

compile1 :: Anno anno => StdPart1 anno -> LyCompile ()
compile1 part = do 
    { let info = initialSectionInfo part
    ; notes <- compilePartToNoteList part
    ; ly <- assembleOutput info notes
    ; writeLyFile (ppRender ly)
    }


-- | Do we want to recalc beams (probably...)

compilePartToNoteList :: Anno anno 
                      => StdPart1 anno -> LyCompile LyNoteListDoc
compilePartToNoteList p = do 
    { p1 <- rebeam p 
    ; p2 <- normalize p1
    ; let info = initialSectionInfo p
    ; p3 <- return $ makeLyNoteListDoc def info p2
    ; return p3
    }
  where
    def = LyOutputDef { printPitch = pitch, printAnno = anno }
    normalize = return . translateToLyPartOut_Relative middle_c
    rebeam s = do { ans <- asksUE ly_recalc_beams
                  ; if ans then (addBeams <=< delBeams) s else return s 
                  }
    -- TEMP
    addBeams = return 
    delBeams = return

    
assembleOutput :: SectionInfo -> LyNoteListDoc -> LyCompile Doc
assembleOutput info notes = do 
    { (title, clef) <- tuneConfig
    ; return $ assembleLy (makeHeader "2.18.6" title) notes
    }
  where
    tuneConfig = (,) <$> asksUE ly_tune_title <*> asksUE ly_clef 
                      
                      

    
-- | Ly has already been rendered to String.
--
writeLyFile :: String -> LyCompile ()
writeLyFile abc = 
    do { root <- getWorkingDirectory
       ; name <- asksUE ly_outfile_name
       ; let outfile = root </> name
       ; liftIO $ writeFile outfile abc
       ; return ()
       }
       