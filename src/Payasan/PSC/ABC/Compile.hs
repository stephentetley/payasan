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
  
  ) where


import Payasan.PSC.CompilerMonad
import Payasan.PSC.Base.SyntaxCommon

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
    { abc_clef          :: !Clef
    , abc_cwd_loc       :: !TempDirLoc
    , abc_outfile_name  :: !String
    }
  deriving (Data,Eq,Show,Typeable)
  
env_zero :: ABCEnv
env_zero = ABCEnv 
    { abc_clef = TREBLE
    , abc_cwd_loc = default_temp_dir_location
    , abc_outfile_name = "abc_output.abc"
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
       