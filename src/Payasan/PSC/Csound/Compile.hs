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



import Payasan.PSC.Base.CompilerMonad
import Payasan.PSC.Base.SyntaxCommon
import Payasan.PSC.Base.Utils
import Payasan.PSC.Repr.External.Syntax




import Text.PrettyPrint.HughesPJ                -- package: pretty


import qualified Data.Text              as TEXT
import qualified Data.Text.IO           as TEXT


import Control.Monad
import Control.Monad.IO.Class
import Data.Data
import System.FilePath



data CompilerDef = CompilerDef
    { pathto_working_dir        :: !FilePath
    , outfile_name              :: !String
    }
  
emptyDef :: CompilerDef
emptyDef = CompilerDef
    { pathto_working_dir        = ""
    , outfile_name              = "cs_output.csd"
    }    
    

data Compiler anno = Compiler
   { compile :: StdPart1 anno -> IO ()
   }

makeCompiler :: Anno anno => CompilerDef -> Compiler anno
makeCompiler env = 
    Compiler { compile = \part -> prompt (compile1 env part)  >> return ()
             }

type CsdCompile a = CM a



compile1 :: Anno anno => CompilerDef -> StdPart1 anno -> CsdCompile ()
compile1 def part = do 
    { events <- compilePartToEventList1 def part
    ; csd <- return (TEXT.pack "") {- return $ assembleOutput1 def notes -}
    ; writeCsdFile1 def csd
    }



compilePartToEventList1 :: CompilerDef -> StdPart1 anno -> CsdCompile LyNoteListDoc
compilePartToEventList1 def p = error "TODO"



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
    do { root <- getWorkingDirectory1 (Right $ pathto_working_dir def) 
       ; let name = outfile_name def
       ; let outfile = root </> name
       ; return outfile
       }

