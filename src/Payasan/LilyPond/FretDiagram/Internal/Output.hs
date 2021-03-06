{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.FretDiagram.Internal.Output
-- Copyright   :  (c) Stephen Tetley 2015-2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Output
--
--------------------------------------------------------------------------------

module Payasan.LilyPond.FretDiagram.Internal.Output
  ( 
    fretDiagramOutput
  ) where

import Payasan.LilyPond.FretDiagram.Internal.Base


import Payasan.PSC.LilyPond.Base
import Payasan.PSC.LilyPond.Pretty
import Payasan.PSC.LilyPond.SimpleOutput

import Payasan.PSC.Repr.External.Syntax (Part, initialSectionInfo)  -- TODO
import Payasan.PSC.Base.SyntaxCommon



import Text.PrettyPrint.HughesPJClass           -- package: pretty



fretDiagramOutput :: String -> String -> [FretDiagram] -> Part LyPitch LyNoteLength FretDiagram -> Doc
fretDiagramOutput lyversion title diags ph = 
        header
    $+$ diag_defs
    $+$ phraseBlock (extractDoc notes)
  where
    diag_defs       = vcat $ map diagramDef diags
    header          = scoreHeader lyversion title
    notes           = makeLyNoteList fret_def ph

    fret_def        :: LyOutputDef LyPitch FretDiagram
    fret_def        = LyOutputDef { printPitch   = pitch
                                  , printAnno    = diagramUse
                                  , partContext  = emptyCtx
                                  }




phraseBlock :: Doc -> Doc
phraseBlock doc  = simultaneous1 $ anonBlock doc




diagramDef :: FretDiagram -> Doc
diagramDef fd@(FretDiagram { fd_name = s }) = 
    text s <+> char '=' <+> block (Just $ command "markup") (pPrint fd)

diagramUse :: FretDiagram -> Doc
diagramUse (FretDiagram { fd_name = s }) = char '^' <> command s
