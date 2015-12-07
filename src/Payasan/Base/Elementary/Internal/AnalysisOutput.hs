{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Elementary.Internal.AnalysisOutput
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Score template with layout context for analysis brackets.
--
--------------------------------------------------------------------------------

module Payasan.Base.Elementary.Internal.AnalysisOutput
  (
    analysisScore
  ) where



import Payasan.Base.Internal.LilyPond.SimpleOutput
import Payasan.Base.Internal.LilyPond.Syntax
import Payasan.Base.Internal.LilyPond.Utils

import Payasan.Base.Internal.BeamSyntax
import Payasan.Base.Internal.CommonSyntax


import Text.PrettyPrint.HughesPJ        -- package: pretty




--------------------------------------------------------------------------------
-- Output

-- TODO - anno actually fixed to Doc
--
analysisScore :: LyOutputDef pch anno 
                    -> ScoreInfo 
                    -> LyPhrase2 pch anno -> Doc
analysisScore def infos ph =
        header $+$ score_ (analysis_layout $+$ lilypondNotes def local1 ph)
  where
    header          = scoreHeader infos
    local1          = maybe default_section_info id $ firstSectionInfo ph


analysis_layout :: Doc
analysis_layout = layout_ $ context_ body
  where
    body = voice_ $+$ consists_ "Horizontal_bracket_engraver"
