{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.FretDiagram.Internal.Output
-- Copyright   :  (c) Stephen Tetley 2015-2016
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

import Payasan.PSC.Repr.External.Syntax (Part, firstSectionInfo)  -- TODO

import Payasan.PSC.Backend.LilyPond.SimpleOutput
import Payasan.PSC.Backend.LilyPond.Utils

import Payasan.PSC.Base.LilyPondCommon
import Payasan.PSC.Base.SyntaxCommon



import Text.PrettyPrint.HughesPJClass           -- package: pretty



fretDiagramOutput :: ScoreInfo -> [FretDiagram] -> Part LyPitch LyNoteLength FretDiagram -> Doc
fretDiagramOutput globals diags ph = 
        header
    $+$ defs defuse
    $+$ phraseBlock (getLyNoteListDoc notes)
  where
    defuse          = diagramDU diags
    header          = scoreHeader globals
    locals1         = maybe default_section_info id $ firstSectionInfo ph
    notes           = lilypondNoteList fret_def locals1 ph

    fret_def        :: LyOutputDef LyPitch FretDiagram
    fret_def        = LyOutputDef { printPitch = pitch
                                  , printAnno  = use defuse }




phraseBlock :: Doc -> Doc
phraseBlock doc  = simultaneous1 $ anonBlock doc



-- | Note - the @universe@ of defs is not closed.
--
-- There are as many defs as there are diagrams defined.
--
diagramDU :: [FretDiagram] -> AnnoDU FretDiagram
diagramDU fs = AnnoDU { defs  = vcat $ map diagramDef fs
                      , use   = diagramUse
                      }

diagramDef :: FretDiagram -> Doc
diagramDef fd@(FretDiagram { fd_name = s }) = 
    text s <+> char '=' <+> block (Just $ command "markup") (pPrint fd)

diagramUse :: FretDiagram -> Doc
diagramUse (FretDiagram { fd_name = s }) = char '^' <> command s
