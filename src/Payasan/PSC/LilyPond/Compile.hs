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
    
    PartCompilerDef(..)
  , emptyDef

  , PartCompiler(..)
  , makePartCompiler

  , LyFile

  , assembleOutput

  , writeLyFile

  ) where


import Payasan.PSC.LilyPond.Base hiding ( middle_c )
import Payasan.PSC.LilyPond.OutTrans
import Payasan.PSC.LilyPond.Pretty
import Payasan.PSC.LilyPond.SimpleOutput

import Payasan.PSC.Base.SyntaxCommon
import Payasan.PSC.Base.Utils
import Payasan.PSC.Repr.External.Syntax
import qualified Payasan.PSC.Repr.External.Syntax as EXT

import Payasan.Base.Duration

import Text.PrettyPrint.HughesPJ               -- package: pretty





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
    , clef_name         :: Maybe String
    , recalc_beams      :: !Bool
    , part_context      :: Doc -> Doc
    } 


emptyDef :: PartCompilerDef ext_pch ly_pch anno
emptyDef = PartCompilerDef
    { pPitch            = mkErr "pPitch"
    , pAnno             = mkErr "pAnno"
    , transformPitch    = mkErr "transformPitch"
    , clef_name         = Just "treble"
    , recalc_beams      = False
    , part_context      = anonBlock
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
    rebeam s  = if recalc_beams lib then (addBeams . delBeams) s else s 
      where
        addBeams  = id  -- TEMP
        delBeams  = id  -- TEMP

    pchTrafo  :: Part ext_pch drn anno -> Part ly_pch drn anno
    pchTrafo  = transformPitch lib

    outDef    :: LyOutputDef ly_pch anno
    outDef    = LyOutputDef { printPitch = pPitch lib 
                            , printAnno  = pAnno lib }

    compileP :: Part ext_pch Duration anno -> LyNoteList
    compileP part = let ext1    = rebeam part
                        extly   = transformLyNoteLength $ pchTrafo ext1
                        out     = makeLyNoteList outDef extly
                    in out



data LyFile_ 
type LyFile = TyDoc LyFile_


assembleOutput :: String -> String -> LyNoteList -> LyFile
assembleOutput version1 title1 notes = 
    TyDoc $ assembleLy (makeLyHeader version1 title1) notes



writeLyFile :: FilePath -> LyFile -> IO ()
writeLyFile path doc = 
    writeFile path (ppRender $ extractDoc doc)
