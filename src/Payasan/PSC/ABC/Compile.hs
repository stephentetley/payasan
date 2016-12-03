{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.ABC.Compile
-- Copyright   :  (c) Stephen Tetley 2016
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
    
    ABCCompile

  , compile

  ) where


import Payasan.PSC.CompilerMonad

import Payasan.PSC.ABC.Output
import Payasan.PSC.ABC.OutTrans
import Payasan.PSC.Base.RewriteMonad
import Payasan.PSC.Base.SyntaxCommon
import Payasan.PSC.Base.Utils
import Payasan.PSC.Repr.External.Syntax


import Text.PrettyPrint.HughesPJ                -- package: pretty

import Control.Monad
import Control.Monad.IO.Class
import Data.Data
import System.FilePath


type ABCCompile a = CM ABCEnv a


-- Note - there is very little user variation that Payasan 
-- should support when generating ABC: 
-- A user might want extra header fields (some are mandatory)
-- and number of bars per line should be allowed but otherwise 
-- ABC output is very regimented.


-- | Note - clef output for abc is part of the 'K' key field, there
-- isn't a simple "clef field".
--
data ABCEnv = ABCEnv 
    { abc_tune_title            :: String
    , abc_clef                  :: !Clef
    , abc_cwd_loc               :: !TempDirLoc
    , abc_outfile_name          :: !String
    , abc_bars_per_line         :: !Int
    , abc_recalc_beams          :: !Bool
    }
  deriving (Data,Eq,Show,Typeable)
  
env_zero :: ABCEnv
env_zero = ABCEnv 
    { abc_tune_title            = "Tune 1"
    , abc_clef                  = TREBLE
    , abc_cwd_loc               = default_temp_dir_location
    , abc_outfile_name          = "abc_output.abc"
    , abc_bars_per_line         = 4
    , abc_recalc_beams          = False    -- default should really be True once we ahve bits in place again
    }    
    
{-

-- This is the existing "compileABC" function from Payasan.PSC.Pipeline
--
-- We want to re-write it to run in the CompilerMonad so it can 
-- use the CompilerMonad's services (logging, errors, ...)
--

outputAsABC :: ScoreInfo -> StaffInfo -> EXT.StdPart1 anno -> String
outputAsABC infos staff = 
    ppRender . ABCOut.abcOutput infos staff
             . ABCOut.translateToABCPartOut
             . addBeams 
             . transExternalToIRBeam
             
-}             

compile :: StdPart1 anno -> IO ()
compile part = runCM env_zero (compile1 part) >>= \ans -> case ans of
    Left err -> putStrLn err
    Right _ -> return ()

compile1 :: StdPart1 anno -> ABCCompile ()
compile1 part = do 
    { let info = initialSectionInfo part
    ; notes <- compilePartToNoteList part
    ; abc <- assembleOutput info notes
    ; writeABCFile (ppRender abc)
    }


-- | Do we want to recalc beams (probably...)

compilePartToNoteList :: StdPart1 anno -> ABCCompile ABCNoteListDoc
compilePartToNoteList p = do 
    { p1 <- rebeam p 
    ; p2 <- normalize p1
    ; let info = initialSectionInfo p
    ; p3 <- rewrite (makeABCNoteListDoc 4 p2) () (stateZero info)
    ; return p3
    }
  where
    normalize = return . translateToABCPartOut
    rebeam s = do { ans <- asksUE abc_recalc_beams
                  ; if ans then (addBeams <=< delBeams) s else return s 
                  }
    -- TEMP
    addBeams = return 
    delBeams = return

    
assembleOutput :: SectionInfo -> ABCNoteListDoc -> ABCCompile Doc
assembleOutput info notes = do 
    { (title, clef) <- tuneConfig
    ; return $ assembleABC (makeHeader title clef info) notes
    }
  where
    tuneConfig = (,) <$> asksUE abc_tune_title <*> asksUE abc_clef 
                      
                      

    
-- | ABC has already been rendered to String.
--
writeABCFile :: String -> ABCCompile ()
writeABCFile abc = 
    do { root <- getTempDirectory =<< asksUE abc_cwd_loc
       ; name <- asksUE abc_outfile_name
       ; let outfile = root </> name
       ; liftIO $ writeFile outfile abc
       ; return ()
       }
       