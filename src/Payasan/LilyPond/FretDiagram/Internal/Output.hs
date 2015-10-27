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
import Payasan.Base.Internal.BeamSyntax (Phrase)

import Payasan.Base.Internal.LilyPond.SimpleOutput ( LyOutputDef(..), renderNotes )
import Payasan.Base.Internal.LilyPond.Syntax
import Payasan.Base.Internal.LilyPond.Utils



import Text.PrettyPrint.HughesPJ        -- package: pretty



fretDiagramOutput :: ScoreInfo -> [FretDiagram] -> Phrase LyPitch LyNoteLength FretDiagramRef -> Doc
fretDiagramOutput globals defs ph = 
        header
    $+$ vsep (map fretDef defs)
    $+$ phraseBlock notes
  where
    header          = oHeader globals
    notes           = renderNotes fret_def ph

    fret_def        :: LyOutputDef LyPitch FretDiagramRef
    fret_def        = LyOutputDef { printPitch = pitch
                                  , printAnno  = anno }



oHeader :: ScoreInfo -> Doc
oHeader globals  = 
        version_ (global_ly_version globals)
    $+$ block (Just $ command "header") (title $ global_title globals)


fretDef :: FretDiagram -> Doc
fretDef = asDefinition

phraseBlock :: Doc -> Doc
phraseBlock doc  = simultaneous1 $ anonBlock doc

