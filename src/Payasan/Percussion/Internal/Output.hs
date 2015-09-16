{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Percussion.Internal.Output
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- (Pipeline)
--
--------------------------------------------------------------------------------

module Payasan.Percussion.Internal.Output
  ( 
    drumsOutput
  ) where

import Payasan.Percussion.Internal.Base

import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.LilyPond.Output ( LyOutputDef(..), renderNotes )
import Payasan.Base.Internal.LilyPond.Utils


import Text.PrettyPrint.HughesPJ        -- package: pretty



drumsOutput :: GlobalRenderInfo -> LyDrumPhrase -> Doc
drumsOutput globals ph = 
        header
    $+$ drumsBlock notes
  where
    header          = oHeader globals
    notes           = renderNotes drum_def ph
    drum_def        = LyOutputDef { pitchPrint = text . shortName }



oHeader :: GlobalRenderInfo -> Doc
oHeader globals  = 
        version (global_ly_version globals)
    $+$ block (Just $ command "header") (title $ global_title globals)


drumsBlock :: Doc -> Doc
drumsBlock doc  = block (Just $ command "drums") doc


-- TODO support percussion-style, bongos-style etc.