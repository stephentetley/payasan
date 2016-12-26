{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Csound.Compile
-- Copyright   :  (c) Stephen Tetley 2016
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
    
    CompilerDef(..)       
  , emptyDef

  , Compiler(..)
  , makeCompiler

  ) where


import Payasan.PSC.Csound.Base

import Payasan.PSC.Base.CompilerMonad
import Payasan.PSC.Base.SyntaxCommon
import Payasan.PSC.Base.Utils
import Payasan.PSC.Repr.External.Syntax




import qualified Data.Text              as TEXT
import qualified Data.Text.IO           as TEXT


import Control.Monad.IO.Class
import System.FilePath



data CompilerDef = CompilerDef
    { pathto_working_dir        :: !FilePath
    , outfile_name              :: !String
    , pathto_csd_template       :: !FilePath
    , template_anchor           :: !String
    }
  
emptyDef :: CompilerDef
emptyDef = CompilerDef
    { pathto_working_dir        = ""
    , outfile_name              = "cs_output.csd"
    , pathto_csd_template       = "./demo/template.csd"
    , template_anchor           = "[|notelist|]"
    }    
    
-- TODO - should provide a method just to compile Part to a doc
-- then this will allow reuse for multi-notelist models 
-- (e.g. polyrhythms)

data Compiler = Compiler
   { compile :: forall anno. StdPart1 anno -> IO ()
   
   }

makeCompiler :: CompilerDef -> Compiler
makeCompiler env = 
    Compiler { compile = \part -> prompt (compile1 env part)  >> return ()
             }

type CsdCompile a = CM a



compile1 :: CompilerDef -> StdPart1 anno -> CsdCompile ()
compile1 def part = do 
    { events <- compilePartToEventList1 def part
    ; csd <- assembleOutput1 def events
    ; writeCsdFile1 def csd
    }



compilePartToEventList1 :: CompilerDef -> StdPart1 anno -> CsdCompile CsdEventListDoc
compilePartToEventList1 def p = error "TODO"


-- This is monadic...
assembleOutput1 :: CompilerDef -> CsdEventListDoc -> CsdCompile TEXT.Text
assembleOutput1 def sco = 
    let scotext = TEXT.pack $ ppRender $ extractDoc sco
    in do { xcsd <- readFileCM (pathto_csd_template def)
          ; return $ TEXT.replace (TEXT.pack $ template_anchor def) scotext xcsd
          }

csoundInsertNotes1 :: CompilerDef -> String -> TEXT.Text -> TEXT.Text
csoundInsertNotes1 def sco = 
    TEXT.replace (TEXT.pack $ template_anchor def) (TEXT.pack sco)


-- | Csd has already been rendered to Text.
--
writeCsdFile1 :: CompilerDef -> TEXT.Text -> CsdCompile ()
writeCsdFile1 def csd = 
    do { outfile <- workingFileName1 def
       ; liftIO $ TEXT.writeFile outfile csd
       ; return ()
       }

workingFileName1 :: CompilerDef -> CsdCompile String
workingFileName1 def = 
    do { root <- getWorkingDirectory (Right $ pathto_working_dir def) 
       ; let name = outfile_name def
       ; let outfile = root </> name
       ; return outfile
       }

