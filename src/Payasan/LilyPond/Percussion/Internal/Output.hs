{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.Percussion.Internal.Output
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

module Payasan.LilyPond.Percussion.Internal.Output
  ( 
    drumsOutput
  ) where

import Payasan.LilyPond.Percussion.Internal.Base

import Payasan.PSC.Backend.LilyPond.SimpleOutput
import Payasan.PSC.Backend.LilyPond.Utils
import Payasan.PSC.Base.SyntaxCommon


import Text.PrettyPrint.HughesPJ        -- package: pretty



drumsOutput :: ScoreInfo -> LyDrumPart -> Doc
drumsOutput _ _ = error "drumsOutput TODO"
{-
drumsOutput globals ph = 
        header
    $+$ drumsBlock notes
  where
    header          = scoreHeader globals
    notes           = lilypondNotes drum_def default_section_info ph
    drum_def        = LyOutputDef { printPitch = text . shortName
                                  , printAnno  = ppAccent }

-}


drumsBlock :: Doc -> Doc
drumsBlock doc  = block (Just $ command "drums") doc

ppAccent :: Accent -> Doc
ppAccent NO_ACCENT = empty
ppAccent ACCENT    = text "->"

-- TODO support percussion-style, bongos-style etc.