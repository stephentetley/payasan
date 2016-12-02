{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Score.Elementary.Internal.AnalysisOutput
-- Copyright   :  (c) Stephen Tetley 2015-2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Score template with layout context for analysis brackets.
--
--------------------------------------------------------------------------------

module Payasan.Score.Elementary.Internal.AnalysisOutput
  (
    analysisScore
  ) where


import Payasan.PSC.Backend.LilyPond.SimpleOutput
import Payasan.PSC.Backend.LilyPond.Utils

import Payasan.PSC.Repr.External.Syntax

import Payasan.PSC.Base.LilyPondCommon
import Payasan.PSC.Base.SyntaxCommon


import Text.PrettyPrint.HughesPJ        -- package: pretty




--------------------------------------------------------------------------------
-- Output

-- TODO - anno actually fixed to Doc
--
analysisScore :: LyOutputDef pch anno 
              -> ScoreInfo 
              -> Part pch LyNoteLength anno -> Doc
analysisScore def infos ph =
    header $+$ score_ (analysis_layout $+$ (getLyNoteListDoc notes))
  where
    header          = scoreHeader infos
    local1          = initialSectionInfo ph
    notes           = lilypondNoteList def local1 ph

analysis_layout :: Doc
analysis_layout = layout_ $ context_ body
  where
    body = voice_ $+$ consists_ "Horizontal_bracket_engraver"
    
    
