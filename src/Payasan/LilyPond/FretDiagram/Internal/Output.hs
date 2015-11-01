{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.FretDiagram.Internal.Output
-- Copyright   :  (c) Stephen Tetley 2015
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
import Payasan.LilyPond.FretDiagram.Internal.Interpret

import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.BeamSyntax (Phrase, firstContextInfo)

import Payasan.Base.Internal.LilyPond.SimpleOutput
import Payasan.Base.Internal.LilyPond.Syntax
import Payasan.Base.Internal.LilyPond.Utils



import Text.PrettyPrint.HughesPJ        -- package: pretty



fretDiagramOutput :: ScoreInfo -> [FretDiagram] -> Phrase LyPitch LyNoteLength FretDiagramRef -> Doc
fretDiagramOutput globals diags ph = 
        header
    $+$ vsep (map fretDef diags)
    $+$ phraseBlock notes
  where
    header          = scoreHeader globals
    locals1         = maybe default_local_info id $ firstContextInfo ph
    notes           = lilypondNotes fret_def locals1 ph

    fret_def        :: LyOutputDef LyPitch FretDiagramRef
    fret_def        = LyOutputDef { printPitch = pitch
                                  , printAnno  = anno }





fretDef :: FretDiagram -> Doc
fretDef = asDefinition

phraseBlock :: Doc -> Doc
phraseBlock doc  = simultaneous1 $ anonBlock doc

