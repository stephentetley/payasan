{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.Tabular.Common
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Output to a Humdrum-like form.
--
-- This is intended debugging and checking purposes, so it is
-- specialized to represent Payasan and is not directly 
-- compatible with Humdrum.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.Tabular.Common
  ( 

    LeafOutput(..)

  , std_ly_output
  , lyNoteLength
  , lyPitch

  , std_abc_output
  , abcNoteLength
  , abcPitch

  , pitch_duration_output

  ) where

import Payasan.Base.Internal.Tabular.Utils

import qualified Payasan.Base.Internal.LilyPond.Syntax as LY
import qualified Payasan.Base.Internal.ABC.Syntax as ABC

import Payasan.Base.Duration
import Payasan.Base.Pitch



import Text.PrettyPrint.HughesPJClass           -- package: pretty

data LeafOutput pch drn anno = LeafOutput 
    { pp_pitch          :: pch -> Doc
    , pp_duration       :: drn -> Doc 
    , pp_anno           :: anno -> Doc
    }



std_ly_output :: LeafOutput LY.LyPitch LY.LyNoteLength ()
std_ly_output = LeafOutput { pp_pitch     = lyPitch
                           , pp_duration  = lyNoteLength
                           , pp_anno      = const empty
                           }

lyNoteLength :: LY.LyNoteLength -> Doc
lyNoteLength (LY.DrnDefault)    = nullDot
lyNoteLength (LY.DrnExplicit d) = duration d

lyPitch :: LY.LyPitch -> Doc
lyPitch = pPrint


std_abc_output :: LeafOutput ABC.ABCPitch ABC.ABCNoteLength ()
std_abc_output = LeafOutput { pp_pitch     = abcPitch
                            , pp_duration  = abcNoteLength
                            , pp_anno      = const empty
                            }

abcNoteLength :: ABC.ABCNoteLength -> Doc
abcNoteLength (ABC.DNL)         = nullDot
abcNoteLength d                 = pPrint d

abcPitch :: ABC.ABCPitch -> Doc
abcPitch = pPrint


pitch_duration_output :: LeafOutput Pitch Duration anno
pitch_duration_output = LeafOutput { pp_pitch     = pPrint
                                   , pp_duration  = pPrint
                                   , pp_anno      = const empty
                                   }


