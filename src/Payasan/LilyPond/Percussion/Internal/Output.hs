{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.Percussion.Internal.Output
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

module Payasan.LilyPond.Percussion.Internal.Output
  ( 
    drumsOutput
  ) where

import Payasan.LilyPond.Percussion.Internal.Base

import Payasan.PSC.LilyPond.Pretty
import Payasan.PSC.LilyPond.SimpleOutput

import Payasan.PSC.Base.SyntaxCommon

import Text.PrettyPrint.HughesPJ        -- package: pretty



drumsOutput :: String -> String -> LyDrumPart -> Doc
drumsOutput lyversion title beampart = 
        header
    $+$ drumsBlock (extractDoc notes)
  where
    header          = scoreHeader lyversion title
    notes           = makeLyNoteList drum_def beampart
    drum_def        = LyOutputDef { printPitch = text . shortName
                                  , printAnno  = ppAccent 
                                  , partContext = emptyCtx
                                  }




drumsBlock :: Doc -> Doc
drumsBlock doc  = block (Just $ command "drums") doc

ppAccent :: Accent -> Doc
ppAccent NO_ACCENT = empty
ppAccent ACCENT    = text "->"

-- TODO support percussion-style, bongos-style etc.