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

  , prompt
  , compile

  , workingFileName

  ) where


import Payasan.PSC.ABC.Output
import Payasan.PSC.ABC.OutTrans

import Payasan.PSC.Base.CompilerMonad
import Payasan.PSC.Base.SyntaxCommon
import Payasan.PSC.Base.Utils
import Payasan.PSC.Repr.External.Syntax



import Control.Monad
import Control.Monad.IO.Class
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
    { abc_tune_title            :: !String
    , abc_clef                  :: !Clef
    , abc_cwd_loc               :: !TempDirLoc
    , abc_outfile_name          :: !String
    , abc_bars_per_line         :: !Int
    , abc_recalc_beams          :: !Bool
    }
  
env_zero :: ABCEnv
env_zero = ABCEnv 
    { abc_tune_title            = "Tune 1"
    , abc_clef                  = TREBLE
    , abc_cwd_loc               = default_temp_dir_location
    , abc_outfile_name          = "abc_output.abc"
    , abc_bars_per_line         = 4
    , abc_recalc_beams          = False    -- default should really be True once we ahve bits in place again
    }    
    


compile :: StdPart1 anno -> IO ()
compile part = prompt env_zero (compile1 part) >> return ()


-- Note - initial section info can fallback to sensible defaults 
-- for an empty score
compile1 :: StdPart1 anno -> ABCCompile ()
compile1 part = do 
    { let info = initialSectionInfo part
    ; header <- makeHeader1 info
    ; notes  <- compilePartToNoteList part
    ; let abc = assembleABC header notes 
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


makeHeader1 :: SectionInfo -> ABCCompile ABCHeader
makeHeader1 info = 
    (\title clef -> makeHeader title clef info) 
        <$> asksUE abc_tune_title <*> asksUE abc_clef 

                      

    
-- | ABC has already been rendered to String.
--
writeABCFile :: String -> ABCCompile ()
writeABCFile abc = 
    do { outfile <- workingFileName
       ; liftIO $ writeFile outfile abc
       ; return ()
       }
       

workingFileName :: ABCCompile String
workingFileName = 
    do { root <- getTempDirectory =<< asksUE abc_cwd_loc
       ; name <- asksUE abc_outfile_name
       ; let outfile = root </> name
       ; return outfile
       }

